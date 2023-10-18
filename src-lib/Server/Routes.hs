{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the route definitions for the visualization server.

module Server.Routes
     ( Routes (..)
     , record
     ) where

import           AppEnv

import           Control.Monad.Except

import           GHC.Generics

import           Lucid

import           Network.HTTP.Media                           ( (//), (/:) )

import           Prelude                                      ()
import           Prelude.Compat

import           Servant                                      as S
import           Servant.Server.Generic

import           Server.Types.Data.StationStatusVisualization
import           Server.Types.Page.StationStatusVisualization


data HTMLLucid
instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml


-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS


-- | Route definitions.
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


