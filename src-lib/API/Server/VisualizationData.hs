{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- FIXME: remove once server is fleshed out a bit more.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- | This module contains the server API to visualize BikeShare data.

module API.Server.VisualizationData
     ( --VisualizationDataAPI
       serveVisualization
       -- , server
     , statusData
     ) where


import           API.Server.Types.Data.StationStatusVisualization
import           API.Server.Types.Page.StationStatusVisualization

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                                  ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time
import           Data.Time.Calendar

import           GHC.Generics

import           Lucid
import           Lucid.Servant

import           Network.HTTP.Media                               ( (//), (/:) )
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Prelude                                          ()
import           Prelude.Compat

import           Servant
import           Servant.Server.Generic
import           Servant.Types.SourceT                            ( source )

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
             , StationStatusVisualization { _statusVisStationId       = 7001
                                          , _statusVisLastReported    = LocalTime (fromGregorian 2000 01 02) (TimeOfDay 00 00 00)
                                          , _statusVisChargingStation = True
                                          , _statusVisBikesAvailable  = 2
                                          , _statusVisBikesDisabled   = 3
                                          , _statusVisDocksAvailable  = 4
                                          , _statusVisDocksDisabled   = 5
                                          , _statusVisAvailableIconic = 6
                                          , _statusVisAvailableEfit   = 7
                                          , _statusVisAvailableEfitG5 = 8
                                          }
             ]
data HTMLLucid
instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

data Routes route where
  Routes :: { _visualizationStationStatus :: route :- "visualization" :> "station-status" :> Get '[HTMLLucid] StationStatusVisualizationPage
            , _dataStationStatus          :: route :- "data" :> "station-status" :> Capture "station-id" Int :> Get '[JSON] [StationStatusVisualization]
            } -> Routes route
  deriving Generic

-- type VisualizationDataAPI = "data" :>
--                                 "station-status" :> Capture "station-id" Int :> Get '[JSON] [StationStatusVisualization]
--                        :<|> "visualization" :>
--                                 "station-status" :> Get '[HTMLLucid] StationStatusVisualizationPage

-- server :: Server (ToServantApi Routes) -- VisualizationDataAPI
-- server = stationStatusData
--          :<|> stationStatusVisualizationPage

--   -- TODO: query database based on 'sId', instead of filtering our example data.
--   where stationStatusData :: Int -> Handler [StationStatusVisualization]
--         stationStatusData sId = return (filter (\sId' -> _statusVisStationId sId' == sId) statusData)

--         stationStatusVisualizationPage :: Handler StationStatusVisualizationPage
--         stationStatusVisualizationPage = return StationStatusVisualizationPage { _statusVisPageStationId = 7001 }


-- visualizationDataAPI :: Proxy VisualizationDataAPI
-- visualizationDataAPI = Proxy

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

-- cliRoutes :: Routes (AsClientT IO)
-- cliRoutes = genericClientHoist
--     (\x -> runClientM x env >>= either throwIO return)
--   where
--     env = error "undefined environment"

-- cliGet :: Int -> IO String
-- cliGet = _get cliRoutes

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks


record :: Routes AsServer
record = Routes
         { _visualizationStationStatus = stationStatusVisualizationPage
         , _dataStationStatus = stationStatusData
         }

stationStatusData :: Int -> Handler [StationStatusVisualization]
stationStatusData sId = return (filter (\sId' -> _statusVisStationId sId' == sId) statusData)

stationStatusVisualizationPage :: Handler StationStatusVisualizationPage
stationStatusVisualizationPage = return StationStatusVisualizationPage { _statusVisPageStationId = 7001 }


-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = genericServe record

serveVisualization :: Int -> IO ()
serveVisualization port = run port app
