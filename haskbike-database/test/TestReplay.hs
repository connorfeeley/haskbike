-- | Test the 'replayed_at' column on the 'queries' table, and the historic (non-demoting)
-- station_information insert path used by the replay subcommand.

module TestReplay
     ( unit_insertStationInformationHistoric
     , unit_insertStationStatusAsOf
     , unit_queryLogReplayedAtColumn
     , unit_queryReplayableQueryLogs
     ) where

import           Control.Lens                                hiding ( reuse, (.=), (<.) )

import           Data.Aeson                                  ( Value (Null), object, (.=) )
import           Data.Functor                                ( void )
import           Data.Int                                    ( Int32 )
import           Data.List                                   ( find )
import           Data.Time

import           Database.Beam                               ( runSelectReturningList, selectWith )
import           Database.Beam.Backend                       ( SqlSerial (..) )

import qualified Haskbike.API.StationInformation             as AT
import           Haskbike.API.Utils                          ( StationStatusSimple (..), baseStatus,
                                                               stationStatusFromSimple )
import           Haskbike.AppEnv                             ( withPostgres )
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Expressions               ( queryLatestInfo, queryLatestStatusLookup )
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.QueryLogs
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Database.Test.Utils

import           Test.Tasty.HUnit

import           UnliftIO


-- * Test units.

-- | HUnit test asserting that a freshly-inserted, previously-unreplayed query log has no
-- 'replayed_at' timestamp.
unit_queryLogReplayedAtColumn :: IO ()
unit_queryLogReplayedAtColumn = withTempDbM Silent setupTestDatabase $ do
  inserted <- insertQueryLog (QueryFailure queriedAt StationStatusEP "boom" (object ["data" .= object []]))
  liftIO $ assertEqual "not replayed" [Nothing] (map _queryLogReplayedAt inserted)
  where
    queriedAt = UTCTime (fromGregorian 2023 10 30) 0

{- | HUnit test for 'insertStationInformationHistoric': an out-of-order (historic) insert must
never demote the currently active row, but a genuinely newer payload still takes the normal
(demoting) path. Also covers that repeating a historic insert is a no-op
(@ON CONFLICT (station_id, reported) DO NOTHING@).
-}
unit_insertStationInformationHistoric :: IO ()
unit_insertStationInformationHistoric = withTempDbM Silent setupTestDatabase $ do
  -- 1. Insert the first (later-reported) row: one active row.
  void $ insertStationInformation [(laterTime, testInfo 20)]

  -- 2. A historic insert of an earlier-reported row must not demote the active (later) row.
  void $ insertStationInformationHistoric [(earlierTime, testInfo 10)]

  rowsAfterHistoric <- queryStationInformationByIds [fromIntegral replayInfoStationId]
  liftIO $ assertEqual "two rows exist after the historic insert" 2 (length rowsAfterHistoric)
  liftIO $ assertEqual "later row is still active"        (Just True)  (activeAt laterTime   rowsAfterHistoric)
  liftIO $ assertEqual "earlier row is inactive history"  (Just False) (activeAt earlierTime rowsAfterHistoric)

  latestAfterHistoric <- withPostgres . runSelectReturningList . selectWith $ queryLatestInfo
  liftIO $ assertEqual "queryLatestInfo still returns the later row"
    [laterTime] (map _infoReported (filter ((== replayInfoStationId) . _infoStationId) latestAfterHistoric))

  -- 3. A genuinely newer payload still takes the normal (demoting) path.
  void $ insertStationInformationHistoric [(evenLaterTime, testInfo 30)]

  rowsAfterNewer <- queryStationInformationByIds [fromIntegral replayInfoStationId]
  liftIO $ assertEqual "three rows exist after the newer insert" 3 (length rowsAfterNewer)
  liftIO $ assertEqual "even-later row is active"               (Just True)  (activeAt evenLaterTime rowsAfterNewer)
  liftIO $ assertEqual "later row was demoted"                  (Just False) (activeAt laterTime     rowsAfterNewer)
  liftIO $ assertEqual "earlier row is still inactive history"  (Just False) (activeAt earlierTime   rowsAfterNewer)

  -- 4. Repeating the historic insert is a no-op.
  (insertedNewer, insertedOlder) <- insertStationInformationHistoric [(earlierTime, testInfo 10)]
  liftIO $ assertEqual "repeating the historic insert inserts no rows via the newer path"   [] insertedNewer
  liftIO $ assertEqual "repeating the historic insert inserts no rows via the history path" [] insertedOlder

  where
    replayInfoStationId :: Int32
    replayInfoStationId = 9101

    earlierTime   = UTCTime (fromGregorian 2024 02 01) (timeOfDayToTime midday)
    laterTime     = UTCTime (fromGregorian 2024 02 02) (timeOfDayToTime midday)
    evenLaterTime = UTCTime (fromGregorian 2024 02 03) (timeOfDayToTime midday)

    activeAt reported rows = _infoActive <$> find ((== reported) . _infoReported) rows

    testInfo capacity =
      AT.StationInformation { AT.infoStationId             = fromIntegral replayInfoStationId
                             , AT.infoName                  = "Historic Insert Test Station"
                             , AT.infoPhysicalConfiguration = Just AT.Regular
                             , AT.infoLat                   = 0.0
                             , AT.infoLon                   = 0.0
                             , AT.infoAltitude              = Just 0
                             , AT.infoAddress               = Just "Nowhere"
                             , AT.infoCapacity              = capacity
                             , AT.infoIsChargingStation     = True
                             , AT.infoRentalMethods         = Nothing
                             , AT.infoIsValetStation        = False
                             , AT.infoIsVirtualStation      = False
                             , AT.infoGroups                = []
                             , AT.infoObcn                  = ""
                             , AT.infoNearbyDistance        = 0
                             , AT.infoBluetoothId           = ""
                             , AT.infoRideCodeSupport       = True
                             , AT.infoRentalUris            = AT.RentalURIs "" "" ""
                             }

{- | HUnit test for 'insertStationStatusAsOf': a status is linked to the station_information row
that was current /as of/ its own @last_reported@ (not the currently active row), and
'station_lookup' only advances forward - a historic (older) as-of insert must not regress the
lookup pointer set by a later live status, but a genuinely newer as-of insert still advances it.
-}
unit_insertStationStatusAsOf :: IO ()
unit_insertStationStatusAsOf = withTempDbM Silent setupTestDatabase $ do
  -- 1. Two info rows for the station: t0 (superseded), t2 (active).
  void $ insertStationInformation [(t0, testInfo 20)]
  void $ insertStationInformation [(t2, testInfo 30)]

  -- 2. A live status at t3 links to the active (t2) row and advances the lookup to t3.
  void $ insertStationStatus [testStatus t3]
  lookupAfterLive <- lookupLastReported
  liftIO $ assertEqual "lookup points at the live status's last_reported" [t3] lookupAfterLive

  -- 3. A historic (as-of) status at t1 (t0 < t1 < t2) must link to the row current as of t1
  -- (t0), and must not regress the lookup pointer already advanced to t3.
  insertedHistoric <- insertStationStatusAsOf [testStatus t1]
  liftIO $ assertEqual "historic status links to the row current as of t1 (t0)"
    [t0] (map (_unInformationReported . _statusInfoId . _statusCommon) insertedHistoric)

  lookupAfterHistoric <- lookupLastReported
  liftIO $ assertEqual "lookup still points at t3 after a historic (older) insert" [t3] lookupAfterHistoric

  -- 4. A genuinely newer as-of status at t4 advances the lookup.
  void $ insertStationStatusAsOf [testStatus t4]
  lookupAfterNewer <- lookupLastReported
  liftIO $ assertEqual "lookup advances to t4 after a newer as-of insert" [t4] lookupAfterNewer

  -- 5. A status reported before every station_information row on record for its station has no
  -- as-of candidate; it is dropped rather than inserted, and must not raise an exception.
  insertedTooEarly <- insertStationStatusAsOf [testStatus tBeforeAnyInfo]
  liftIO $ assertEqual "a status older than all known info rows is dropped, not inserted" [] insertedTooEarly

  where
    asOfStationId :: Int32
    asOfStationId = 7001

    tBeforeAnyInfo = addUTCTime (-3600) t0
    t0 = UTCTime (fromGregorian 2024 03 01) (timeOfDayToTime midday)
    t1 = UTCTime (fromGregorian 2024 03 02) (timeOfDayToTime midday)
    t2 = UTCTime (fromGregorian 2024 03 03) (timeOfDayToTime midday)
    t3 = UTCTime (fromGregorian 2024 03 04) (timeOfDayToTime midday)
    t4 = UTCTime (fromGregorian 2024 03 05) (timeOfDayToTime midday)

    lookupLastReported = map (^. statusLastReported) <$> (withPostgres . runSelectReturningList . selectWith $ queryLatestStatusLookup Nothing)

    testStatus lastReported =
      stationStatusFromSimple baseStatus (StationStatusSimple (fromIntegral asOfStationId) lastReported 0 0 0 0 0 0 0 0)

    testInfo capacity =
      AT.StationInformation { AT.infoStationId             = fromIntegral asOfStationId
                             , AT.infoName                  = "As-Of Insert Test Station"
                             , AT.infoPhysicalConfiguration = Just AT.Regular
                             , AT.infoLat                   = 0.0
                             , AT.infoLon                   = 0.0
                             , AT.infoAltitude              = Just 0
                             , AT.infoAddress               = Just "Nowhere"
                             , AT.infoCapacity              = capacity
                             , AT.infoIsChargingStation     = True
                             , AT.infoRentalMethods         = Nothing
                             , AT.infoIsValetStation        = False
                             , AT.infoIsVirtualStation      = False
                             , AT.infoGroups                = []
                             , AT.infoObcn                  = ""
                             , AT.infoNearbyDistance        = 0
                             , AT.infoBluetoothId           = ""
                             , AT.infoRideCodeSupport       = True
                             , AT.infoRentalUris            = AT.RentalURIs "" "" ""
                             }

{- | HUnit test for 'queryReplayableQueryLogs' and 'markQueryLogReplayedPg': only failed queries
with a real (non-null) captured error body and no 'replayed_at' are replay candidates, returned
oldest first; marking one replayed removes it from the pending set; and each optional filter
(endpoint, since, until, id, limit) narrows the result as expected.
-}
unit_queryReplayableQueryLogs :: IO ()
unit_queryReplayableQueryLogs = withTempDbM Silent setupTestDatabase $ do
  -- Noise: a success (excluded - it did not fail) and a failure with a JSON null body
  -- (excluded - there is nothing to replay).
  void $ insertQueryLog (QuerySuccess successTime infoEndpoint)
  void $ insertQueryLog (QueryFailure nullBodyTime infoEndpoint "boom" Null)

  -- Two replayable failures, inserted out of chronological order (t2 before t1).
  rowsAtT2 <- insertQueryLog (QueryFailure t2 statusEndpoint "boom2" (object ["data" .= (2 :: Int)]))
  rowsAtT1 <- insertQueryLog (QueryFailure t1 infoEndpoint   "boom1" (object ["data" .= (1 :: Int)]))
  let rowAtT2 = head rowsAtT2
      rowAtT1 = head rowsAtT1

  pending <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "pending replayable logs, oldest first" [t1, t2] (map _queryLogTime pending)

  -- Endpoint filter narrows to the matching row.
  pendingByEndpoint <- queryReplayableQueryLogs (Just infoEndpoint) Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "endpoint filter narrows to the matching row" [t1] (map _queryLogTime pendingByEndpoint)

  -- since/until filters narrow to the matching row.
  pendingSince <- queryReplayableQueryLogs Nothing (Just t2) Nothing Nothing Nothing
  liftIO $ assertEqual "since filter excludes the earlier row" [t2] (map _queryLogTime pendingSince)

  pendingUntil <- queryReplayableQueryLogs Nothing Nothing (Just t1) Nothing Nothing
  liftIO $ assertEqual "until filter excludes the later row" [t1] (map _queryLogTime pendingUntil)

  -- id filter narrows to the matching row.
  pendingById <- queryReplayableQueryLogs Nothing Nothing Nothing (Just (unSerial (_queryLogId rowAtT2))) Nothing
  liftIO $ assertEqual "id filter narrows to the matching row" [t2] (map _queryLogTime pendingById)

  -- limit narrows to the oldest row.
  pendingLimited <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing (Just 1)
  liftIO $ assertEqual "limit narrows to the oldest row" [t1] (map _queryLogTime pendingLimited)

  -- Marking t1 replayed removes it from the pending set; t2 remains.
  void $ withPostgres (markQueryLogReplayedPg (_queryLogId rowAtT1) markedAt)
  pendingAfterMark <- queryReplayableQueryLogs Nothing Nothing Nothing Nothing Nothing
  liftIO $ assertEqual "only t2 remains pending after marking t1 replayed" [t2] (map _queryLogTime pendingAfterMark)

  where
    successTime  = UTCTime (fromGregorian 2024 04 01) (timeOfDayToTime midday)
    nullBodyTime = UTCTime (fromGregorian 2024 04 02) (timeOfDayToTime midday)
    t1           = UTCTime (fromGregorian 2024 04 03) (timeOfDayToTime midday)
    t2           = UTCTime (fromGregorian 2024 04 04) (timeOfDayToTime midday)
    markedAt     = UTCTime (fromGregorian 2024 04 05) (timeOfDayToTime midday)

    infoEndpoint   = StationInformationEP
    statusEndpoint = StationStatusEP
