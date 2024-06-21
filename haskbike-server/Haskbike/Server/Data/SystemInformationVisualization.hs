-- | Module for generating the data for the SystemInformation visualization.

module Haskbike.Server.Data.SystemInformationVisualization where

import           Control.Monad.Catch                            ( MonadCatch )

import           Data.Aeson
import           Data.Time

import           GHC.Generics                                   ( Generic )

import           Haskbike.AppEnv
import           Haskbike.Database.Operations.SystemInformation
import           Haskbike.Database.Tables.SystemInformation

import           UnliftIO


-- | Type representing a the visualization data for a BikeShare station's status.
data SystemInformationCountVisualization where
  SystemInformationCountVisualization :: { _sysInfCntVisReported        :: UTCTime
                                         , _sysInfCntVisStationCount    :: Int
                                         , _sysInfCntVisMechanicalCount :: Int
                                         , _sysInfCntVisEbikeCount      :: Int
                                         } -> SystemInformationCountVisualization
  deriving (Show, Generic, Eq, Ord)

instance ToJSON SystemInformationCountVisualization where
  toJSON station =
    object [ "Reported"         .= _sysInfCntVisReported        station
           , "Station Count"    .= _sysInfCntVisStationCount    station
           , "Mechanical Count" .= _sysInfCntVisMechanicalCount station
           , "E-Bike Count"     .= _sysInfCntVisEbikeCount      station
           ]

-- | Convert from the Beam StationStatus type to SystemInformationVisualization
fromBeamSysInfoCntToVisJSON :: SystemInformationCount -> SystemInformationCountVisualization
fromBeamSysInfoCntToVisJSON sysInfCnt =
  SystemInformationCountVisualization { _sysInfCntVisReported        = (_sysInfKeyReported . _sysInfCntKey) sysInfCnt
                                      , _sysInfCntVisStationCount    = fromIntegral (_sysInfCntStationCount sysInfCnt)
                                      , _sysInfCntVisMechanicalCount = fromIntegral (_sysInfCntMechanicalCount sysInfCnt)
                                      , _sysInfCntVisEbikeCount      = fromIntegral (_sysInfCntEbikeCount sysInfCnt)
                                      }


generateJsonDataSourceSysInfo :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                              => Maybe LocalTime -> Maybe LocalTime -> m [SystemInformationCountVisualization]
generateJsonDataSourceSysInfo startTime endTime = do
  result <- queryLatestSystemInfo startTime endTime

  pure $ map fromBeamSysInfoCntToVisJSON result
