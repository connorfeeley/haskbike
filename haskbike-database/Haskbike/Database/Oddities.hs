-- | This module contains a collection of queries that are used to extract oddities from the BikeShare database.

module Haskbike.Database.Oddities
     ( analyzeStationStatusCapacityMismatch
     , queryStationStatusCapacityMismatchE
     ) where

import           Colog

import           Control.Arrow                               ( (&&&) )
import           Control.Lens                                hiding ( reuse, (<.) )
import           Control.Monad
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Int                                    ( Int32 )
import           Data.List
import qualified Data.Text                                   as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg
import           Database.Beam.Postgres.Full                 hiding ( insert )

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Operations.Utils
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus


{-
The sum of the station status':
- bikes available
- bikes disabled
- docks available
- docks disabled

Should be equal to the station information's capacity.

Query where this axiom is violated (in a range).

>>> res@(count, info, status) <- unzip3 <$> ((Haskbike.AppEnv.runWithAppMDebug "haskbike" . withPostgres . runSelectReturningList . selectWith) $ queryStationStatusCapacityMismatch 7001 (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 06 00 00))) (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 06 45 00))))
-}
queryStationStatusCapacityMismatchE :: Maybe Int32 -> UTCTime -> UTCTime
                                    -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (QGenExpr QValueContext Postgres s Int32, StationInformationT (QGenExpr QValueContext Postgres s), StationStatusT (QGenExpr QValueContext Postgres s)))
queryStationStatusCapacityMismatchE stationId startTime endTime = do
  -- Query status in range.
  statusQuery <- selecting $
                 Pg.pgNubBy_ (_statusLastReported . _statusCommon &&& (_unInformationStationId . _statusInfoId . _statusCommon)) $
                 orderBy_ (asc_ . _statusLastReported . _statusCommon) $
                 filter_' (\status ->
                            sqlBool_ (stationIdCond stationId status) &&?.
                            sqlBool_ (between_ (status ^. statusLastReported) (val_ startTime) (val_ endTime))
                         )
                 (all_ (bikeshareDb ^. bikeshareStationStatus))
  pure $ do
    status <- reuse statusQuery
    allStatusCount <- aggregate_ (\_ -> as_ @Int32 countAll_) $ reuse statusQuery

    -- Query all station information.
    info   <- lateral_ status $ \status' -> do
                related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status')

    -- Get only the status rows where the sum of the status fields is not equal to the capacity.
    guard_' (sumStatus status /=?. _infoCapacity info)

    -- Return the mismatched information and status, along with the original number of status rows queried.
    pure (allStatusCount, info, status)


-- | Helper function to sum the variable components of the station status.
sumStatus :: Num (Columnar f1 Int32) => StationStatusT f1 -> Columnar f1 Int32
sumStatus status = status ^. statusNumBikesAvailable +
                   status ^. statusNumBikesDisabled +
                   status ^. statusNumDocksAvailable +
                   status ^. statusNumDocksDisabled

-- | Query for the station status capacity mismatch and analyze the results.
-- analyzeStationStatusCapacityMismatch :: Maybe Int32 -> UTCTime -> UTCTime -> m ()
analyzeStationStatusCapacityMismatch :: (HasEnv env m, MonadCatch m) => Maybe Int32 -> UTCTime -> UTCTime -> m [(Int32, StationInformation, StationStatus)]
analyzeStationStatusCapacityMismatch stationId startTime endTime = do
  _mismatched@(count, info, status) <- unzip3 <$> (withPostgres . runSelectReturningList . selectWith)
    (queryStationStatusCapacityMismatchE stationId startTime endTime)
  processMismatchedRecords $ zip3 count info status
  pure $ zip3 count info status

processMismatchedRecords :: (Monad m, HasEnv env m) => [(Int32, StationInformation, StationStatus)] -> m ()
processMismatchedRecords mismatched = do
  logInfo $ "Queried " <> (T.pack . show . maximum) count <> " status rows total"
  logInfo $ (T.pack . show . length) mismatched <> " status rows didn't match capacity"
  logInfo $ "Min capacity: " <> (T.pack . show . minimum) (map _infoCapacity info)
  logInfo $ "Max capacity: " <> (T.pack . show . maximum) (map _infoCapacity info)
  logInfo "Frequency of sums: "
  forM_ frequencies $ \freq ->
    logInfo $ "    " <> showFreq freq
  -- Log the okay rows returned
  logInfo $ "    " <> (T.pack . show . maximum) (map _infoCapacity info) <> ": " <> (T.pack . show) rowsOkay <> " times"

  -- forM_ mismatched $ \(count, info, status) -> do
  --   logInfo . T.pack $ "Status:"
  --   logInfo . T.pack $ "    Frequency of sums: " <> show (frequency (map sumStatus status))
  --   logInfo . T.pack $ "Capacity: " ++ show (_infoCapacity info)
  --   logInfo . T.pack $ "Status count: " ++ show count
  --   logInfo . T.pack $ "Status sum: " ++ show (sumStatus status)
  --   logInfo . T.pack $ "Status: " ++ show status
  --   logInfo . T.pack $ ""
  where
    rowsOkay = (fromIntegral . maximum $ count) - length mismatched
    count = map (view _1) mismatched
    info = map (view _2) mismatched
    status = map (view _3) mismatched
    frequencies = frequency (map sumStatus status)

showFreq :: (Ord a, Show a) => (Int, a) -> T.Text
showFreq (count, x) = T.pack $ show x <> ": " <> show count <> " times"

frequency :: Ord a => [a] -> [(Int,a)]
frequency = map (length &&& head) . group . sort
