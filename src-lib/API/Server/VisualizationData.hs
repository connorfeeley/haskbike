{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- |

module API.Server.VisualizationData
     ( StationStatusVisualization (..)
     , VisualizationDataAPI
     , app
     , main
     , server
     , statusData
     , visualizationDataAPI
     ) where


import           API.Server.Types.StationStatusVisualization

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                             ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time
import           Data.Time.Calendar

import           GHC.Generics

import           Lucid

import           Network.HTTP.Media                          ( (//), (/:) )
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Prelude                                     ()
import           Prelude.Compat

import           Servant
import           Servant.Types.SourceT                       ( source )

import           System.Directory

import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8


-- | Example data.
statusData :: [StationStatusVisualization]
statusData = [ StationStatusVisualization { _statusVisStationId       = 7000
                                          , _statusVisLastReported    = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
                                          , _statusVisChargingStation = False
                                          , _statusVisBikesAvailable  = 1
                                          , _statusVisBikesDisabled   = 2
                                          , _statusVisDocksAvailable  = 3
                                          , _statusVisDocksDisabled   = 4
                                          , _statusVisAvailableIconic = 6
                                          , _statusVisAvailableEfit   = 7
                                          , _statusVisAvailableEfitG5 = 8
                                          }
             , StationStatusVisualization { _statusVisStationId       = 7001
                                          , _statusVisLastReported    = LocalTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00)
                                          , _statusVisChargingStation = True
                                          , _statusVisBikesAvailable  = 1
                                          , _statusVisBikesDisabled   = 2
                                          , _statusVisDocksAvailable  = 3
                                          , _statusVisDocksDisabled   = 4
                                          , _statusVisAvailableIconic = 6
                                          , _statusVisAvailableEfit   = 7
                                          , _statusVisAvailableEfitG5 = 8
                                          }
             ]

type VisualizationDataAPI = "station-status" :> Capture "station-id" Int :> Get '[JSON] [StationStatusVisualization]

server :: Server VisualizationDataAPI
server = stationStatusHandler

  -- TODO: query database based on 'sId', instead of filtering our example data.
  where stationStatusHandler :: Int -> Handler [StationStatusVisualization]
        stationStatusHandler sId = return (filter (\sId' -> _statusVisStationId sId' == sId) statusData)


visualizationDataAPI :: Proxy VisualizationDataAPI
visualizationDataAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve visualizationDataAPI server

main :: IO ()
main = run 8081 app
