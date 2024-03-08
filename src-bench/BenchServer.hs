-- | Benchmark suite.

module BenchServer where

import           AppEnv

import           Control.Monad                              ( void )

import           Data.Time

import           Database.Beam
import           Database.BikeShare.Connection
import           Database.BikeShare.Operations.StationEmpty

import           Utils


benchStationEmptyTime :: Maybe Int -> IO ()
benchStationEmptyTime station = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase
  initDBWithAllTestData

  void $ runWithAppM "haskbike" $ withPostgres $ runSelectReturningList $ selectWith $ queryStationEmptyFullTime station
    (UTCTime (fromGregorian 2023 11 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 11 02) (timeOfDayToTime midnight))

-- Here's our Servant Client function
-- apiClient :: Client ClientM (NamedRoutes VisualizationAPI)
-- apiClient = client (Proxy @(NamedRoutes VisualizationAPI)) // stationEmptyFullList /: Nothing

-- benchStationEmptyFullPage :: IO ()
-- benchStationEmptyFullPage = do
--   liftIO $ void $ concurrently (runWithServerAppM "haskbike" serveVisualization)
--                                (runWithServerAppM "haskbike" serveVisualization)
