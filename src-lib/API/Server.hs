{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the server API to visualize BikeShare data.

module API.Server
     ( apiProxy
     , routesLinks
     , serveVisualization
     ) where

import           API.Server.Types.Data.StationStatusVisualization
import           API.Server.Types.Page.StationStatusVisualization

import           AppEnv

import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.ByteString.Lazy                             as BL
import           Data.Proxy
import           Data.Time
import           Data.Time.Extras

import           GHC.Generics

import           Lucid

import           Network.HTTP.Media                               ( MediaType, (//), (/:) )
import           Network.Wai.Handler.Warp                         as Warp

import           Prelude                                          ()
import           Prelude.Compat

import           Servant                                          as S
import           Servant.Server.Generic


data HTMLLucid
instance Accept HTMLLucid where
    contentType :: Proxy HTMLLucid -> MediaType
    contentType _ = "text" // "html" /: ("charset", "utf-8")
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender :: ToHtml a => Proxy HTMLLucid -> a -> BL.ByteString
    mimeRender _ = renderBS . toHtml


-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender :: Proxy HTMLLucid -> Html a -> BL.ByteString
    mimeRender _ = renderBS


-- | Route definitions.
data Routes route where
  Routes :: { _visualizationStationStatus :: route :- "visualization"
                                                :> "station-status"
                                                        :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
                                                        :> Get '[HTMLLucid] StationStatusVisualizationPage
            , _dataStationStatus          :: route :- "data"
                                                :> "station-status"
                                                        :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
                                                        :> Get '[JSON] [StationStatusVisualization]
            } -> Routes route
  deriving Generic

record :: Routes (AsServerT AppM)
record =
  Routes { _visualizationStationStatus = stationStatusVisualizationPage
         , _dataStationStatus          = stationStatusData
         }

stationStatusData :: Int -> Maybe LocalTime -> Maybe LocalTime -> AppM [StationStatusVisualization]
stationStatusData = generateJsonDataSource

stationStatusVisualizationPage :: Int -> Maybe LocalTime -> Maybe LocalTime -> AppM StationStatusVisualizationPage
stationStatusVisualizationPage stationId startTime endTime =
  return StationStatusVisualizationPage { _statusVisPageStationId = stationId
                                        , _statusVisPageTimeRange = TimePair startTime endTime
                                        }

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks



apiProxy :: Proxy (ToServantApi Routes)
apiProxy = genericApi (Proxy :: Proxy Routes)


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

