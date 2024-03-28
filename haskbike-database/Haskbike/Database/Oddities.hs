-- | This module contains a collection of queries that are used to extract oddities from the BikeShare database.

module Haskbike.Database.Oddities
     ( queryStationStatusCapacityMismatchE
     ) where

import           Control.Arrow                               ( (&&&) )
import           Control.Lens                                hiding ( reuse, (<.) )
import           Control.Monad                               ( forM_ )

import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg
import           Database.Beam.Postgres.Full                 hiding ( insert )

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
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
queryStationStatusCapacityMismatchE :: Int32 -> UTCTime -> UTCTime
                                       -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (QGenExpr QValueContext Postgres s Int32, StationInformationT (QGenExpr QValueContext Postgres s), StationStatusT (QGenExpr QValueContext Postgres s)))
queryStationStatusCapacityMismatchE stationId startTime endTime = do
  -- Query status in range.
  statusQuery <- selecting $
                 Pg.pgNubBy_ (_statusLastReported . _statusCommon &&& (_unInformationStationId . _statusInfoId . _statusCommon)) $
                 orderBy_ (asc_ . _statusLastReported . _statusCommon) $
                 filter_ (\status -> (_unInformationStationId . _statusInfoId . _statusCommon) status ==. val_ stationId &&.
                         between_ (status ^. statusLastReported) (val_ startTime) (val_ endTime))
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
analyzeStationStatusCapacityMismatch stationId startTime endTime = do
  res@(count, info, status) <- unzip3 <$> ((withPostgres . runSelectReturningList . selectWith) $
                                           queryStationStatusCapacityMismatchE 7395
                                           (UTCTime (fromGregorian 2024 03 01) (timeOfDayToTime midnight))
                                           (UTCTime (fromGregorian 2024 03 28) (timeOfDayToTime midnight)))
  -- forM_ (count, info, status) $ \(count', info', status') -> do
  --   liftIO $ putStrLn $ "Mismatched station status capacity for station " ++ show (_unInformationStationId info') ++ " at " ++ show (_statusLastReported $ _statusCommon status') ++ " with " ++ show count ++ " status rows."
  --   liftIO $ putStrLn $ "Station information: " ++ show info'
  --   liftIO $ putStrLn $ "Station status: " ++ show status'
  --   liftIO $ putStrLn ""
  pure ()
