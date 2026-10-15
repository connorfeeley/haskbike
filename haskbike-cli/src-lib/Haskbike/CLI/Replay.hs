{- | Replay failed API queries recorded in the @queries@ table: re-decode each query's stored
response body, insert it with the same historically-correct linkage the live poller would have
used, and mark the query log replayed.
-}

module Haskbike.CLI.Replay
     ( ReplaySkip (..)
     , ReplaySummary (..)
     , dispatchReplay
     , formatReplaySummary
     , replayPayloadPg
     , replayQueryLog
     ) where

import           Colog                                  ( logError, logException, logInfo, logWarning )

import           Control.Lens                           ( (^.) )
import           Control.Monad                          ( foldM, when )
import           Control.Monad.Catch                    ( MonadCatch, MonadThrow )

import           Data.Aeson                             ( FromJSON, Result (..), Value (..), fromJSON )
import qualified Data.ByteString.Char8                  as BS
import           Data.Maybe                             ( isJust )
import qualified Data.Text                              as T
import           Data.Text.Lazy                         ( Text, pack )
import           Data.Time                              ( getCurrentTime )

import           Database.Beam.Postgres                 ( Pg, SqlError (sqlErrorMsg) )

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation        as AT
import qualified Haskbike.API.StationStatus             as AT
import qualified Haskbike.API.SystemInformation         as AT
import           Haskbike.AppEnv                        hiding ( Error )
import           Haskbike.CLI.Options                   ( ReplayOptions (..) )
import           Haskbike.CLI.QueryFormat               ( boldCode, cliOut, colouredText, resetIntens, withHeader )
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations           ( insertStationInformationHistoricPg, insertStationStatusAsOfPg,
                                                          insertSystemInformationPg )
import           Haskbike.Database.Operations.QueryLogs ( decodeJsonError, markQueryLogReplayedPg,
                                                          queryReplayableQueryLogs )
import           Haskbike.Database.Tables.QueryLogs     ( QueryLog, queryLogEndpoint, queryLogErrJson, queryLogId )

import           Prelude                                hiding ( log )

import           System.Console.ANSI                    ( Color (White), ColorIntensity (Vivid) )
import           System.Exit                            ( exitFailure )

import           UnliftIO                               ( MonadIO, MonadUnliftIO, liftIO, try )


-- | Tally of what happened while replaying a batch of failed query logs.
data ReplaySummary where
  ReplaySummary :: { rsConsidered  :: !Int
                   , rsReplayed    :: !Int
                   , rsRows        :: !Int
                   , rsUndecodable :: !Int
                   , rsUnsupported :: !Int
                   , rsFailed      :: !Int
                   } -> ReplaySummary
  deriving (Eq, Show)

instance Semigroup ReplaySummary where
  left <> right = ReplaySummary
    { rsConsidered  = rsConsidered  left + rsConsidered  right
    , rsReplayed    = rsReplayed    left + rsReplayed    right
    , rsRows        = rsRows        left + rsRows        right
    , rsUndecodable = rsUndecodable left + rsUndecodable right
    , rsUnsupported = rsUnsupported left + rsUnsupported right
    , rsFailed      = rsFailed      left + rsFailed      right
    }

instance Monoid ReplaySummary where
  mempty = ReplaySummary 0 0 0 0 0 0

-- | Why a query log's stored body could not be replayed.
data ReplaySkip where
  Undecodable :: String          -> ReplaySkip
  Unsupported :: EndpointQueried -> ReplaySkip
  deriving (Show)

{- | Build the 'Pg' action that inserts a query log's decoded payload, or explain why it can't be
replayed. The returned 'Pg' action's 'Int' is the count of rows it inserts.
-}
replayPayloadPg :: EndpointQueried -> Value -> Either ReplaySkip (Pg Int)
replayPayloadPg StationInformationEP body = do
  response <- decodeStored body :: Either ReplaySkip (ResponseWrapper [AT.StationInformation])
  let historicPayload = map (_respLastUpdated response,) (_respData response)
  pure $ do
    (newerRows, olderRows) <- insertStationInformationHistoricPg historicPayload
    pure (length newerRows + length olderRows)
replayPayloadPg StationStatusEP body = do
  response <- decodeStored body :: Either ReplaySkip (ResponseWrapper [AT.StationStatus])
  pure (length <$> insertStationStatusAsOfPg (_respData response))
replayPayloadPg SystemInformationEP body = do
  response <- decodeStored body :: Either ReplaySkip (ResponseWrapper AT.SystemInformation)
  pure (insertSystemInformationPg (_respLastUpdated response) (_respData response))
replayPayloadPg unsupportedEndpoint _body = Left (Unsupported unsupportedEndpoint)

{- | Decode a stored @error_json@ value as the type the endpoint's response should have. A value
that is syntactically JSON but fails to decode as that type is 'Undecodable'.
-}
decodeStored :: FromJSON a => Value -> Either ReplaySkip a
decodeStored value = case fromJSON value of
  Error decodeErrorMessage -> Left (Undecodable decodeErrorMessage)
  Success decoded          -> Right decoded

{- | Replay one failed query log: decode its stored body, insert it, and mark it replayed, all in
one transaction, so an insert failure after a successful decode leaves @replayed_at@ unset. A dry
run only decodes and counts; it makes no database change. A 'SqlError' from the transaction is
caught and counted rather than propagated, so one bad row doesn't abort the whole batch.
-}
replayQueryLog :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
               => Bool -> QueryLog -> m ReplaySummary
replayQueryLog isDryRun queryLog =
  case replayPayloadPg endpoint payloadValue of
    Left (Undecodable decodeErrorMessage) -> do
      logWarning $ "Replay: query log " <> queryLogIdText <> " has an undecodable body: " <> T.pack decodeErrorMessage
      pure consideredUndecodable
    Left (Unsupported unsupportedEndpoint) -> do
      logWarning $ "Replay: query log " <> queryLogIdText <> " targets an unsupported endpoint: " <> T.pack (show unsupportedEndpoint)
      pure consideredUnsupported
    Right insertPayload
      | isDryRun  -> do
          logInfo $ "Replay (dry run): would replay query log " <> queryLogIdText
          pure (consideredReplayed 0)
      | otherwise -> replayAndMark insertPayload
  where
    endpoint       = queryLog ^. queryLogEndpoint
    payloadValue   = maybe Null decodeJsonError (queryLog ^. queryLogErrJson)
    queryLogRowId  = queryLog ^. queryLogId
    queryLogIdText = T.pack (show queryLogRowId)

    replayAndMark insertPayload = do
      replayedAt <- liftIO getCurrentTime
      attempt <- try . withPostgresTransaction $ do
        insertedRowCount <- insertPayload
        _                <- markQueryLogReplayedPg queryLogRowId replayedAt
        pure insertedRowCount
      case attempt of
        Left (sqlError :: SqlError) -> do
          logException sqlError
          logError $ "Replay: failed to replay query log " <> queryLogIdText
                  <> ": " <> T.pack (BS.unpack (sqlErrorMsg sqlError))
          pure consideredFailed
        Right insertedRowCount -> pure (consideredReplayed insertedRowCount)

consideredReplayed :: Int -> ReplaySummary
consideredReplayed insertedRowCount =
  ReplaySummary { rsConsidered = 1, rsReplayed = 1, rsRows = insertedRowCount, rsUndecodable = 0, rsUnsupported = 0, rsFailed = 0 }

consideredUndecodable :: ReplaySummary
consideredUndecodable =
  ReplaySummary { rsConsidered = 1, rsReplayed = 0, rsRows = 0, rsUndecodable = 1, rsUnsupported = 0, rsFailed = 0 }

consideredUnsupported :: ReplaySummary
consideredUnsupported =
  ReplaySummary { rsConsidered = 1, rsReplayed = 0, rsRows = 0, rsUndecodable = 0, rsUnsupported = 1, rsFailed = 0 }

consideredFailed :: ReplaySummary
consideredFailed =
  ReplaySummary { rsConsidered = 1, rsReplayed = 0, rsRows = 0, rsUndecodable = 0, rsUnsupported = 0, rsFailed = 1 }

-- | Replay every pending failed query matching the options, then print a summary.
--
-- A @--limit@ below 1 exits non-zero so scripts don't mistake a rejected argument for a clean run.
-- That check really belongs in the option parser; move it there when convenient.
dispatchReplay :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
               => ReplayOptions -> m ()
dispatchReplay options
  | Just limitValue <- optReplayLimit options, limitValue < 1 = do
      logError $ "Replay: --limit must be at least 1, got " <> T.pack (show limitValue)
      liftIO exitFailure
  | otherwise = do
      candidates <- queryReplayableQueryLogs
        (optReplayEndpoint options) (optReplaySince options) (optReplayUntil options)
        (optReplayId options) (optReplayLimit options)
      logInfo $ "Replay: " <> T.pack (show (length candidates)) <> " query log(s) pending replay."
      when (isJust (optReplayId options) && null candidates) $
        logWarning "Replay: --id matched no pending query log."
      summary <- foldM accumulate mempty candidates
      liftIO $ cliOut (formatReplaySummary (optReplayDryRun options) summary)
  where
    accumulate accumulated queryLog = (accumulated <>) <$> replayQueryLog (optReplayDryRun options) queryLog

{- | Render a 'ReplaySummary' for 'cliOut'. The header is labelled "(dry run)" when the summary
came from a dry run, so an operator never mistakes its "Replayed" count for rows actually written.
-}
formatReplaySummary :: Bool -> ReplaySummary -> [Text]
formatReplaySummary isDryRun summary =
  withHeader (pack headerText)
    [ summaryLine "Considered"  (rsConsidered summary)
    , summaryLine "Replayed"    (rsReplayed summary)
    , summaryLine "Rows"        (rsRows summary)
    , summaryLine "Undecodable" (rsUndecodable summary)
    , summaryLine "Unsupported" (rsUnsupported summary)
    , summaryLine "Failed"      (rsFailed summary)
    ]
  where
    headerText = "Replay Summary" <> if isDryRun then " (dry run)" else ""

    summaryLine :: String -> Int -> Text
    summaryLine label count =
      boldCode <> colouredText Vivid White (pack label <> ":\t") <> resetIntens <> pack (show count)
