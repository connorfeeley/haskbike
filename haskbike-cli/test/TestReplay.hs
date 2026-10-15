-- | Test the 'replay' subcommand's option parsing and its query-log replay behaviour.

module TestReplay
     ( tests
     ) where

import           Control.Monad                          ( foldM, void )

import           Data.Aeson                             ( object, toJSON, (.=) )
import           Data.Time

import           Database.PostgreSQL.Simple             ( execute_ )

import           GHC.Exts                               ( fromString )

import           Haskbike.API.ResponseWrapper           ( ResponseWrapper (..) )
import           Haskbike.API.Utils                     ( StationStatusSimple (..), baseStatus,
                                                          stationStatusFromSimple )
import           Haskbike.AppEnv                        ( withPooledConn )
import           Haskbike.CLI.Options                   ( Command (..), ReplayOptions (..), commandParser )
import           Haskbike.CLI.Replay                    ( ReplaySummary (..), replayQueryLog )
import           Haskbike.Database.BikeShare
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations           ( insertStationInformation, queryRowCount )
import           Haskbike.Database.Operations.QueryLogs ( insertQueryLog, queryReplayableQueryLogs )
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.Database.Test.Utils

import           Options.Applicative

import           Test.Tasty
import           Test.Tasty.HUnit

import           UnliftIO                               ( liftIO )


tests :: TestTree
tests = testGroup "Replay option parsing tests"
  [ testCase "Replay options parse" replayOptionsParseWithFlags
  , testCase "Replay options parse defaults" replayOptionsParseWithDefaults
  , testCase "Replay inserts a decodable failed query" replayInsertsDecodableFailedQuery
  , testCase "Replay leaves an undecodable body pending" replayLeavesUndecodableBodyPending
  , testCase "Replay dry run does not mark or insert" replayDryRunDoesNotMarkOrInsert
  , testCase "Replay records a failing query and continues" replayRecordsFailingQueryAndContinues
  ]

-- | Parsing @replay --endpoint station-status --limit 5 --dry-run@ yields the expected 'ReplayOptions'.
replayOptionsParseWithFlags :: IO ()
replayOptionsParseWithFlags =
  assertReplayOptionsParseTo ["replay", "--endpoint", "station-status", "--limit", "5", "--dry-run"] expectedOptions
  where
    expectedOptions = ReplayOptions { optReplayEndpoint = Just StationStatusEP
                                     , optReplaySince    = Nothing
                                     , optReplayUntil    = Nothing
                                     , optReplayLimit    = Just 5
                                     , optReplayId       = Nothing
                                     , optReplayDryRun   = True
                                     }

-- | Parsing @replay@ with no flags yields every 'Maybe' field unset and 'optReplayDryRun' clear.
replayOptionsParseWithDefaults :: IO ()
replayOptionsParseWithDefaults =
  assertReplayOptionsParseTo ["replay"] expectedOptions
  where
    expectedOptions = ReplayOptions { optReplayEndpoint = Nothing
                                     , optReplaySince    = Nothing
                                     , optReplayUntil    = Nothing
                                     , optReplayLimit    = Nothing
                                     , optReplayId       = Nothing
                                     , optReplayDryRun   = False
                                     }

-- | Assert that parsing the given command-line arguments produces the 'Replay' command
-- carrying exactly the given 'ReplayOptions'.
assertReplayOptionsParseTo :: [String] -> ReplayOptions -> IO ()
assertReplayOptionsParseTo args expectedOptions =
  case execParserPure defaultPrefs (info commandParser mempty) args of
    Success (Replay parsedOptions) -> parsedOptions @?= expectedOptions
    Success _otherCommand          -> assertFailure "expected the 'Replay' command"
    Failure failure                -> assertFailure (fst (renderFailure failure "haskbike-cli"))
    CompletionInvoked _completion  -> assertFailure "expected a parse result, got a completion request"

-- | Replaying a failed query whose stored body decodes successfully inserts its rows, marks it
-- replayed, and is no longer returned by 'queryReplayableQueryLogs'.
replayInsertsDecodableFailedQuery :: IO ()
replayInsertsDecodableFailedQuery = withTempDbM Silent migrateDB $ do
  infoResponse <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  loggedTime   <- liftIO getCurrentTime
  void $ insertQueryLog (QueryFailure loggedTime StationInformationEP "decode" (toJSON infoResponse))

  [queryLog] <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  summary    <- replayQueryLog False queryLog
  liftIO $ assertEqual "Replayed the query log"        1 (rsReplayed summary)

  infoRowCount    <- queryRowCount bikeshareStationInformation
  liftIO $ assertBool "Inserted station information rows" (infoRowCount > 0)

  stillPending <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "No query logs left pending" 0 (length stillPending)

-- | Replaying a failed query whose stored body is valid JSON but not the endpoint's response shape
-- counts it as undecodable and leaves it pending (@replayed_at@ unset).
replayLeavesUndecodableBodyPending :: IO ()
replayLeavesUndecodableBodyPending = withTempDbM Silent migrateDB $ do
  loggedTime <- liftIO getCurrentTime
  void $ insertQueryLog (QueryFailure loggedTime StationInformationEP "decode" (object ["nonsense" .= True]))

  [queryLog] <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  summary    <- replayQueryLog False queryLog
  liftIO $ assertEqual "Counted as undecodable" 1 (rsUndecodable summary)

  stillPending <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "Query log is still pending" 1 (length stillPending)

-- | A dry run decodes and counts a decodable failed query as replayed, but inserts nothing and
-- leaves the query log pending.
replayDryRunDoesNotMarkOrInsert :: IO ()
replayDryRunDoesNotMarkOrInsert = withTempDbM Silent migrateDB $ do
  infoResponse <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  loggedTime   <- liftIO getCurrentTime
  void $ insertQueryLog (QueryFailure loggedTime StationInformationEP "decode" (toJSON infoResponse))

  [queryLog] <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  summary    <- replayQueryLog True queryLog
  liftIO $ assertEqual "Dry run counts as replayed" 1 (rsReplayed summary)

  infoRowCount <- queryRowCount bikeshareStationInformation
  liftIO $ assertEqual "Dry run does not insert" 0 infoRowCount

  stillPending <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "Dry run leaves the query log pending" 1 (length stillPending)

{- | A per-row insert failure (a CHECK constraint mimicking "no partition for this row") is caught,
counted, and leaves that query pending, without aborting the batch or poisoning the connection for
the next query: a second, healthy query replays normally afterward.
-}
replayRecordsFailingQueryAndContinues :: IO ()
replayRecordsFailingQueryAndContinues = withTempDbM Silent migrateDB $ do
  -- 1. Seed station_information for one station so station_status inserts can link.
  void $ insertStationInformation [(infoTime, manualStationInformation)]

  -- 2. Two decodable station_status bodies for that station: A (logged first) reports after the
  -- cutoff and will violate the CHECK constraint added below; B (logged second) reports before it
  -- and will insert cleanly.
  void $ insertQueryLog (QueryFailure loggedAtA StationStatusEP "decode" (toJSON (statusResponseAt failingTime)))
  void $ insertQueryLog (QueryFailure loggedAtB StationStatusEP "decode" (toJSON (statusResponseAt succeedingTime)))

  -- 3. Add a CHECK constraint mimicking a production "no partition for row" failure: any status
  -- reported at or after the cutoff violates it, so replaying query A's insert throws a 'SqlError'.
  void $ withPooledConn (\conn () -> execute_ conn constraintSql) ()

  -- 4. Replay both pending queries, oldest logged first (A, then B).
  pendingBeforeReplay <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  summary <- foldM (\accumulated queryLog -> (accumulated <>) <$> replayQueryLog False queryLog) mempty pendingBeforeReplay

  -- 5. One failure (A), one success (B); A stays pending, B is inserted and marked replayed.
  liftIO $ assertEqual "One query failed the CHECK constraint (A)" 1 (rsFailed summary)
  liftIO $ assertEqual "One query replayed successfully (B)"       1 (rsReplayed summary)

  stillPending <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "Only the failing query (A) is still pending" [loggedAtA] (map _queryLogTime stillPending)

  statusRowCount <- queryRowCount bikeshareStationStatus
  liftIO $ assertEqual "Only B's status row was inserted" 1 statusRowCount

  where
    infoTime       = UTCTime (fromGregorian 2024 01 01) 0
    cutoffTime     = UTCTime (fromGregorian 2024 06 01) 0
    failingTime    = addUTCTime 3600    cutoffTime
    succeedingTime = addUTCTime (-3600) cutoffTime

    loggedAtA = UTCTime (fromGregorian 2025 01 01) 0
    loggedAtB = UTCTime (fromGregorian 2025 01 02) 0

    constraintSql =
      fromString ("ALTER TABLE station_status ADD CONSTRAINT replay_test_window CHECK (last_reported < '" <> show cutoffTime <> "')")

    statusResponseAt lastReported = ResponseWrapper
      { _respLastUpdated = lastReported
      , _respTtl         = 30
      , _respVer         = "2.3"
      , _respData        = [stationStatusFromSimple baseStatus (StationStatusSimple 7001 lastReported 10 0 0 10 0 0 0 0)]
      }
