-- | Benchmark suite.

module BenchServer where

import           AppEnv

import           Colog.Core                                   ( Severity (..) )

import           Control.Monad                                ( void )

import qualified Data.ByteString.Lazy.Char8                   as BSC
import           Data.Data                                    ( Proxy (..) )
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as T
import           Data.Time

import           Database.Beam
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus
import           Database.BikeShare.Utils

import           Servant.API                                  ( MimeUnrender (..), NamedRoutes )
import           Servant.Client                               ( AsClientT, ClientM, HasClient (..), client, (//), (/:) )
import           Servant.HTML.Lucid

import           Server
import           Server.Page.List.Common                      ( StationList )
import           Server.Page.List.StationEmptyFullList        ( EmptyFull )
import           Server.Page.SideMenu                         ( PureSideMenu )
import           Server.Routes                                ( API, BikeShareExplorerAPI )
import           Server.VisualizationAPI

import           ServerEnv

import           Test.Tasty.Bench

import           TestChargings

import           UnliftIO

import           Utils


fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ -- bgroup "Fibonacci numbers"
    -- [ bench "fifth"     $ nf fibo  5
    -- , bench "tenth"     $ nf fibo 10
    -- , bench "twentieth" $ nf fibo 20
    -- ],
    bgroup "Servant server"
      [ -- bench "Query bike chargings" $ whnfIO unit_queryChargings
      -- ,
        bench "Station empty time" $ whnfIO benchStationEmptyFullPage
      ]
  ]

benchStationEmptyTime :: IO ()
benchStationEmptyTime = do
  void $ runWithAppM "haskbike" $ withPostgres $ runSelectReturningList $ selectWith $ queryStationEmptyFullTime Nothing (UTCTime (fromGregorian 2024 01 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2024 01 12) (timeOfDayToTime midnight))

-- Here's our Servant Client function
-- apiClient :: Client ClientM (NamedRoutes VisualizationAPI)
-- apiClient = client (Proxy @(NamedRoutes VisualizationAPI)) // stationEmptyFullList /: Nothing

benchStationEmptyFullPage :: IO ()
benchStationEmptyFullPage = do
  liftIO $ void $ concurrently (runWithServerAppM "haskbike" serveVisualization)
                               (runWithServerAppM "haskbike" serveVisualization)
 -- void $ runWithAppM "haskbike" $ withPostgres $ runSelectReturningList $ selectWith $ queryStationEmptyFullTime (Just 7001) (UTCTime (fromGregorian 2024 01 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2024 01 12) (timeOfDayToTime midnight))
