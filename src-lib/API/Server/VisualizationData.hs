{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- FIXME: remove once server is fleshed out a bit more.


-- | This module contains the server API to visualize BikeShare data.

module API.Server.VisualizationData
     ( serveVisualization
     ) where

import           API.Server.Types.Data.StationStatusVisualization
import           API.Server.Types.Page.StationStatusVisualization

import           AppEnv

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.List
import           Data.Time

import           GHC.Generics

import           Lucid

import           Network.HTTP.Media                               ( (//), (/:) )
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Prelude                                          ()
import           Prelude.Compat

import           Servant
import           Servant.Server.Generic


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

record :: Routes (AsServerT AppM)
record = Routes
         { _visualizationStationStatus = stationStatusVisualizationPage
         , _dataStationStatus = stationStatusData
         } where
                stationStatusData :: Int -> AppM [StationStatusVisualization]
                stationStatusData = generateJsonDataSource

                stationStatusVisualizationPage :: AppM StationStatusVisualizationPage
                stationStatusVisualizationPage = return StationStatusVisualizationPage { _statusVisPageStationId = 7001 }

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks


-- | Natural transformation function for AppM monad
nt :: Env AppM -> AppM a -> Handler a
nt s a =
  let r = runReaderT (unAppM a) s
  in liftIO r


app :: Env AppM -> Application
app s = -- serve api $ hoistServer api (nt s) server
  genericServeT (nt s) record


serveVisualization :: Int -> AppM ()
serveVisualization port = do
  env <- ask
  liftIO $ run port (app env)
