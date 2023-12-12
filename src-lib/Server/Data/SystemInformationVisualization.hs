-- |

module Server.Data.SystemInformationVisualization where

import           AppEnv

import           Control.Monad.Except

import           Data.Aeson
import           Data.Time

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.Tables.SystemInformation

import           ServerEnv



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

generateJsonDataSourceSysInfo :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM [SystemInformationCountVisualization]
generateJsonDataSourceSysInfo _startTime _endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- getAppEnvFromServer
  result <- liftIO $ runAppM appEnv $ withPostgres $
    runSelectReturningList $ selectWith queryLatestSystemInfo

  pure $ map fromBeamSysInfoCntToVisJSON result
