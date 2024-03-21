{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Test the database.

module TestDatabase
     ( getDecodedFileSystemInformation
     , initDBWithAllTestData
     , setupTestDatabase
     , unit_insertNewerStatusRecords
     , unit_insertNewerStatusRecordsInsert
     , unit_insertNewerStatusRecordsInsertTwice
     , unit_insertStationApi
     , unit_insertStationInformation
     , unit_insertStationInformationApi
     , unit_insertStationInformationDuplicate
     , unit_insertStationLookupLatest
     , unit_insertStationStatus
     , unit_insertStationStatusApi
     , unit_insertSystemInformation
     , unit_queryDockingUndockingCount
     , unit_queryStationByIdAndName
     , unit_queryStationStatus
     , unit_queryStationStatusBetween
     ) where

import           Control.Lens                                hiding ( reuse )
import           Control.Monad                               ( unless, (<=<) )
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Functor                                ( void )
import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.SystemInformation              as AT
import           Haskbike.AppEnv
import           Haskbike.Database.EventCounts
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Database.Tables.SystemInformation
import           Haskbike.Database.Test.Utils
import           Haskbike.Database.Utils

import           Paths_haskbike_database                     ( getDataFileName )

import           Test.Tasty.HUnit

import           UnliftIO                                    ( MonadUnliftIO, bracket, try )


-- | HUnit test for inserting system information.
unit_insertSystemInformation :: IO ()
unit_insertSystemInformation = do
  status  <- getDecodedFileSystemInformation "test/json/system_information.json"
  let (reported, info) = (status ^. respLastUpdated, status ^. respData)

  -- Connect to the database.
  (insertedInfo, insertedInfoCount) <- withTempDbM Silent setupTestDatabase $ do
    -- Should fail because station information has not been inserted.
    -- Catch exception with 'try'.
    insertSystemInformation reported info


  assertEqual "Inserted system information length" (1, 1) (length insertedInfo, length insertedInfoCount)

  assertEqual "Inserted system information" expectedInfo (fromBeamSystemInformationToJSON (head insertedInfo) (head insertedInfoCount))
  where
    expectedInfo = AT.SystemInformation { AT._sysInfStationCount          = 756
                                        , AT._sysInfVehicleCount          = AT.SystemInformationVehicleCount 8126 825
                                        , AT._sysInfBuildHash             = "2a5b9d6"
                                        , AT._sysInfBuildLabel            = "2023-11-17"
                                        , AT._sysInfBuildNumber           = "267"
                                        , AT._sysInfBuildVersion          = "2023.1"
                                        , AT._sysInfLanguage              = "en"
                                        , AT._sysInfMobileHeadVersion     = 2
                                        , AT._sysInfMobileMinSuppVersion  = 1
                                        , AT._sysInfName                  = "bike_share_toronto"
                                        , AT._sysInfSysId                 = "bike_share_toronto"
                                        , AT._sysInfTimeZone              = "America/Toronto"
                                        }


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  info <- getDecodedFileInformation "test/json/station_information.json"

  inserted_info <- withTempDbM Silent setupTestDatabase $ do
    -- Insert test data.
    insertStationInformation (_respLastUpdated info) (_respData info)

  assertEqual "Inserted station information" 7 (length inserted_info)


-- | HUnit test for inserting station information where only the reported time has changed.
unit_insertStationInformationDuplicate :: IO ()
unit_insertStationInformationDuplicate = do
  -- Connect to the database.
  withTempDbM Silent setupTestDatabase $ do
    -- Insert first record. Should insert.
    insertedInfo <- insertStationInformation firstTime [testInfo]
    liftIO $ assertEqual "Inserted station information" 1 (length insertedInfo)

    -- Insert first record again with same reported time (no other changes). Should not insert.
    insertedInfo' <- insertStationInformation firstTime [testInfo]
    liftIO $ assertEqual "Inserted station information" 0 (length insertedInfo')

    -- Insert first record again with different reported time (no other changes). Should not insert.
    insertedInfo'' <- insertStationInformation secondTime [testInfo]
    liftIO $ assertEqual "Inserted station information" 0 (length insertedInfo'')

  -- TODO: bluetooth_id is flapping.

  where
    firstTime  = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight)
    secondTime = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midday)
    testInfo =
      AT.StationInformation { AT.infoStationId             = 6000
                            , AT.infoName                  = "Test Station"
                            , AT.infoPhysicalConfiguration = AT.Regular
                            , AT.infoLat                   = 0.0
                            , AT.infoLon                   = 0.0
                            , AT.infoAltitude              = Just 0
                            , AT.infoAddress               = Just "Nowhere"
                            , AT.infoCapacity              = 1
                            , AT.infoIsChargingStation     = True
                            , AT.infoRentalMethods         = []
                            , AT.infoIsValetStation        = False
                            , AT.infoIsVirtualStation      = False
                            , AT.infoGroups                = []
                            , AT.infoObcn                  = ""
                            , AT.infoNearbyDistance        = 0
                            , AT.infoBluetoothId           = ""
                            , AT.infoRideCodeSupport       = True
                            , AT.infoRentalUris            = AT.RentalURIs "" "" ""
                            }



-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  info    <- getDecodedFileInformation "test/json/station_information-1.json"
  status  <- getDecodedFileStatus      "test/json/station_status.json"

  withTempDbM Silent setupTestDatabase $ do

    -- Insert test data.
    insertedInfo   <- insertStationInformation (_respLastUpdated info) (_respData info)
    insertedStatus <- insertStationStatus $ status ^. respData

    liftIO $ assertEqual "Inserted station information" 704 (length insertedInfo)
    liftIO $ assertEqual "Inserted station status"        8 (length insertedStatus)


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  info    <- getDecodedFileInformation  "test/json/station_information-1.json"
  status  <- getDecodedFileStatus       "test/json/station_status.json"

  withTempDbM Silent setupTestDatabase $ do
    -- Insert test data.
    insertedInfo   <- insertStationInformation (_respLastUpdated info) (_respData info)
    insertedStatus <- insertStationStatus $ status ^. respData

    liftIO $ assertEqual "Inserted station information" 704 (length insertedInfo)
    liftIO $ assertEqual "Inserted station status"        8 (length insertedStatus)

    -- Query station status.
    queryStationStatus (Just 1000) >>= liftIO . (assertEqual "Query status (limit: 1000)" 8 . length)
    queryStationStatus Nothing     >>= liftIO . (assertEqual "Query status (limit: none)" 8 . length)


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  info    <- getDecodedFileInformation "test/json/station_information-1.json"
  withTempDbM Silent setupTestDatabase $ do
    -- Insert test data.
    void $ insertStationInformation (_respLastUpdated info) (_respData info)


-- | HUnit test for inserting station status, with data from the actual API.
unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  status  <- getDecodedFileStatus "test/json/station_status-1.json"
  withTempDbM Silent setupTestDatabase $ do
    -- Should fail because station information has not been inserted.
    -- Catch exception with 'try'.
    insertedStatus <- try $
      insertStationStatus $ status ^. respData

    -- Exception was expected - only return error if inserted succeeded.
    -- If the insertion succeeded and is not length 0, then database schema does not enforce foreign key constraint correctly.
    case insertedStatus of
      Left (_ :: SqlError) -> pure ()
      Right inserted       ->
        unless (null inserted)
        (liftIO $ assertFailure ("Was able to insert status records without information populated: " <> (show . length) inserted <> " inserted"))

-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  info    <- getDecodedFileInformation "test/json/station_information-1.json"
  status  <- getDecodedFileStatus      "test/json/station_status-1.json"
  withTempDbM Silent setupTestDatabase $ do
    -- Insert test data.
    inserted_info   <- insertStationInformation (_respLastUpdated info) (_respData info)
    inserted_status <- insertStationStatus $ status ^. respData

    liftIO $ assertEqual "Inserted station information" 704 (length inserted_info)
    liftIO $ assertEqual "Inserted station status"      704 (length inserted_status)


{- | HUnit test for querying which station status have reported.

Between /station_status-1/ and /station_status-2/, station 7000 reported new data but 7001 did not:
+---------+--------------------+--------------------+-----+
| Station | last\_reported (1) | last\_reported (2) |   Δ |
+=========+====================+====================+=====+
|    7000 |         1694798090 |         1694798350 | 260 |
+---------+--------------------+--------------------+-----+
|    7001 |         1694798218 |         1694798218 |   0 |
+---------+--------------------+--------------------+-----+
-}
unit_insertNewerStatusRecords :: IO ()
unit_insertNewerStatusRecords = do
  withTempDbM Silent setupTestDatabase $ do
    -- Separate API status records into those that are newer than in the database entry and those that are unchanged.
    inserted <- doInsertNewerStatusRecords

    liftIO $ assertEqual "No API status records newer than database entries" 302 (length inserted)

    -- Station 7000 should be in the list of API records that would trigger a database update, but not in the list of unchanged records.
    liftIO $ assertBool "Station 7000 record is newer"          (has (traverse . statusInfoId . unInformationStationId . only 7000) inserted)

    -- Station 7001 should be in the list of API records that would /not/ trigger a database update, but not in the list of newer records.
    liftIO $ assertBool "Station 7001 record is unchanged" (not (has (traverse . statusInfoId . unInformationStationId . only 7001) inserted))

doInsertNewerStatusRecords :: (HasEnv env m, MonadCatch m) => m [StationStatus]
doInsertNewerStatusRecords = do
  info      <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  status_1  <- liftIO $ getDecodedFileStatus      "test/json/station_status-1.json"
  status_2  <- liftIO $ getDecodedFileStatus      "test/json/station_status-2.json"

  -- Insert test data.
  void $ insertStationInformation (_respLastUpdated info) (_respData info)
  void $ insertStationStatus      $ status_1 ^. respData

  -- Return inserted station status.
  insertStationStatus $ status_2 ^. respData


-- | HUnit test to assert that changed station status are inserted.
unit_insertNewerStatusRecordsInsert :: IO ()
unit_insertNewerStatusRecordsInsert = do
  withTempDbM Silent setupTestDatabase $ do
    {-
    - Insert information and status data (1)
    - Insert/update status data (2)
    - Check if inserting status data (2) would result in updates
    - Returns (updated, same)
    -}
    updated <- doStatusInsertOnce

    -- Assert that the same number of status rows are inserted as were updated.
    liftIO $ assertEqual "Inserted status rows"        302 (length updated)

-- | Insert station statuses (1) into a database, then (2).
doStatusInsertOnce :: (HasEnv env m, MonadCatch m) => m [StationStatus] -- ^ Result of inserting station statuses.
doStatusInsertOnce = do
  info      <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  status_1  <- liftIO $ getDecodedFileStatus      "test/json/station_status-1.json"
  status_2  <- liftIO $ getDecodedFileStatus      "test/json/station_status-2.json"

  -- Insert test data.
  void $ insertStationInformation (_respLastUpdated info) (_respData info)
  void $ insertStationStatus      $ status_1 ^. respData

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  insertStationStatus $ status_2 ^. respData


-- | HUnit test to assert that reinserting rows is a no-op.
unit_insertNewerStatusRecordsInsertTwice :: IO ()
unit_insertNewerStatusRecordsInsertTwice = do
  withTempDbM Silent setupTestDatabase $ do
    inserted <- doStatusInsertTwice

    -- Assert that the no status rows were inserted on the second iteration.
    liftIO $ assertEqual "Reinserting duplicate statuses inserts 0 new status rows"          0 (length inserted)


-- | Insert station statuses (1) into a database, then (2), then (2) again.
doStatusInsertTwice :: (HasEnv env m, MonadCatch m) => m [StationStatus] -- ^ Result of inserting updated station statuses.
doStatusInsertTwice = do
  info      <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  status_1  <- liftIO $ getDecodedFileStatus "test/json/station_status-1.json"
  status_2  <- liftIO $ getDecodedFileStatus "test/json/station_status-2.json"

  -- Insert first round of test data.
  void $ insertStationInformation (_respLastUpdated info) (_respData info)
  void $ insertStationStatus      $ status_1 ^. respData

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  void $ insertStationStatus $ status_2 ^. respData

  -- Insert second round of test data once again (nothing should have changed).
  insertStationStatus $ status_2 ^. respData


-- | HUnit test to validate that a station ID can be looked up by its name, and vice-versa.
unit_queryStationByIdAndName :: IO ()
unit_queryStationByIdAndName = do
  info <- getDecodedFileInformation "test/json/station_information-1.json"
  withTempDbM Silent setupTestDatabase $ do
    void $ insertStationInformation (_respLastUpdated info) (_respData info)

    liftIO . assertEqual "Station ID for 'King St W / Joe Shuster Way'" (Just 7148)  =<< queryStationId "King St W / Joe Shuster Way"
    liftIO . assertEqual "Station ID for 'Wellesley Station Green P'" (Just 7001)    =<< queryStationId "Wellesley Station Green P"
    liftIO . assertEqual "Stations with name ending in 'Green P'"
      [ (7001,"Wellesley Station Green P")
      , (7050,"Richmond St E / Jarvis St Green P")
      , (7112,"Liberty St / Fraser Ave Green P")
      , (7789,"75 Holly St - Green P")
      ] =<< queryStationIdLike "%Green P"


-- | HUnit test to query all status records for a station between two times.
unit_queryStationStatusBetween :: IO ()
unit_queryStationStatusBetween = withTempDbM Silent (setupTestDatabase >> initDBWithAllTestData) $ do
    -- First status for #7001 was inserted at 2023-09-15 17:16:58; last status at 2023-09-15 17:35:00.
    statusBetweenAll <- queryStationStatusBetween 7001
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:16:58")))
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:35:00")))
    liftIO $ assertEqual "Expected number of status records for #7001 between two valid times" 4 (length statusBetweenAll)

    -- Query for status records for #7001 between two times, where the start and end time match the first status report.
    statusBetweenFirst <- queryStationStatusBetween 7001
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:16:58"))) -- Moment the first status was reported.
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:16:58"))) -- Moment the first status was reported.
    liftIO $ assertEqual "Expected number of status records for #7001 for first status reported" 1 (length statusBetweenFirst)

    -- Query for status records for #7001 between two times, where the end time is before the first status was reported.
    statusBetweenTooEarly <- queryStationStatusBetween 7001
      (UTCTime (read "2000-01-01") (timeOfDayToTime (read "00:00:00")))-- Arbitrary date
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:16:57")))-- One second before first status reported.
    liftIO $ assertEqual "Expected number of status records for #7001 before first status reported" 0 (length statusBetweenTooEarly)

    {-
    Query for status records for #7001 between two times, where the earliest time is *after* the first status was reported,
    and the end time is *before* the first status was reported.
    -}
    statusBetweenBackwards <- queryStationStatusBetween 7001
      (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:16:59"))) -- One second after first status reported.
      (UTCTime (read "2000-01-01") (timeOfDayToTime (read "00:00:00"))) -- Arbitrary date
    liftIO $ assertEqual "Expected number of status records for #7001 with backwards time parameters" 0 (length statusBetweenBackwards)


-- | HUnit test to query all status records for a station between two times.
unit_queryDockingUndockingCount :: IO ()
unit_queryDockingUndockingCount = withTempDbM Silent (setupTestDatabase >> initDBWithAllTestData) $ do
    -- Test dataset has 5 rows for station 7000, 3 rows for 7006, 5 for 7012, and 5 rows for 7148:
    -- |   id | station | reported               | iconic |
    -- |------+---------+------------------------+--------|
    -- |    1 |    7000 | 2023-09-15 17:14:50-04 |      0 |
    -- |  705 |    7000 | 2023-09-15 17:19:10-04 |      0 |
    -- | 1309 |    7000 | 2023-09-15 17:23:29-04 |      5 |
    -- | 1849 |    7000 | 2023-09-15 17:32:06-04 |     26 |
    -- | 2746 |    7000 | 2023-09-15 17:36:27-04 |     29 |
    -- |------+---------+------------------------+--------|
    -- |    7 |    7006 | 2023-09-15 17:17:15-04 |     13 |
    -- | 1164 |    7006 | 2023-09-15 17:21:36-04 |     10 |
    -- | 1855 |    7006 | 2023-09-15 17:34:31-04 |      7 |
    -- |------+---------+------------------------+--------|
    -- |   12 |    7012 | 2023-09-15 17:14:33-04 |     10 |
    -- |  710 |    7012 | 2023-09-15 17:18:52-04 |     11 |
    -- | 1317 |    7012 | 2023-09-15 17:23:09-04 |     13 |
    -- | 1860 |    7012 | 2023-09-15 17:31:47-04 |     12 |
    -- | 2749 |    7012 | 2023-09-15 17:36:06-04 |     12 |
    -- |------+---------+------------------------+--------|
    -- |  136 |    7148 | 2023-09-15 17:14:12-04 |     17 |
    -- |  757 |    7148 | 2023-09-15 17:18:40-04 |     17 |
    -- | 1411 |    7148 | 2023-09-15 17:23:08-04 |     17 |
    -- | 1983 |    7148 | 2023-09-15 17:32:07-04 |     19 |
    -- | 2849 |    7148 | 2023-09-15 17:36:37-04 |     19 |

    --              station  thresholds     dockings undockings
    checkConditions 7000     thresholds7000 29         0
    checkConditions 7006     thresholds7006  0       (-6)
    checkConditions 7012     thresholds7012  3       (-1)
    checkConditions 7148     thresholds7148  2         0
  where
    thresholds7000  = [ EarliestTime (UTCTime (read "2023-09-01") (timeOfDayToTime midnight)), LatestTime (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:36:27.0"))) ]
    thresholds7006  = [ EarliestTime (UTCTime (read "2023-09-01") (timeOfDayToTime midnight)), LatestTime (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:34:31.0"))) ]
    thresholds7012  = [ EarliestTime (UTCTime (read "2023-09-01") (timeOfDayToTime midnight)), LatestTime (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:36:06.0"))) ]
    thresholds7148  = [ EarliestTime (UTCTime (read "2023-09-01") (timeOfDayToTime midnight)), LatestTime (UTCTime (read "2023-09-15") (timeOfDayToTime (read "17:36:37.0"))) ]


checkConditions :: (HasEnv env m, MonadCatch m, MonadUnliftIO m) => Int32 -> [StatusThreshold] -> Int -> Int -> m ()
checkConditions stationId thresholds expectDockings expectUndockings = do
  eventCountsForStation <- findInList stationId <$> queryDockingEventsCount (StatusVariationQuery (Just stationId) thresholds)
  liftIO $ assertEqual ("Expected number of undockings at station " ++ show stationId)
    (Just expectUndockings)
    (_eventsCountUndockings . _eventsIconicCount  <$> eventCountsForStation)
  liftIO $ assertEqual ("Expected number of dockings at station "   ++ show stationId)
    (Just expectDockings)
    (_eventsCountDockings   . _eventsIconicCount  <$> eventCountsForStation)

findInList :: Int32 -> [DockingEventsCount] -> Maybe DockingEventsCount
findInList key tuples = tuples ^? folded . filtered (\k -> k ^. eventsStation . infoStationId == key)

-- | HUnit test for inserting station status and asserting that the lookup table is accurate.
unit_insertStationLookupLatest :: IO ()
unit_insertStationLookupLatest = do
  info    <- getDecodedFileInformation "test/json/station_information-1.json"
  status  <- getDecodedFileStatus      "test/json/station_status-1.json"
  withTempDbM Silent setupTestDatabase $ do
    -- Insert test data.
    insertedInfo   <- insertStationInformation (_respLastUpdated info) (_respData info)
    insertedStatus <- insertStationStatus (status ^. respData)

    statusLookup <- withPostgres $ runSelectReturningList $ selectWith $ queryLatestStatusLookup Nothing
    infoLookup   <- withPostgres $ runSelectReturningList $ selectWith $ queryLatestInfoLookup   Nothing

    liftIO $ assertEqual "Status lookup length" (length insertedStatus) (length statusLookup)
    liftIO $ assertEqual "Inserted station status is same as latest status" insertedStatus statusLookup

    liftIO $ assertEqual "Info lookup length" (length insertedInfo) (length infoLookup)
    liftIO $ assertEqual "Inserted station information is same as latest info" insertedInfo infoLookup
