-- |

module API.Poll where

import           API.Client
import           API.ResponseWrapper
import           API.Types                       (StationStatusResponse)
import           Common

import           Servant.Client

import           Control.Applicative             ((<|>))
import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception               (finally)
import           Control.Monad                   (forever, void)
import           Data.Foldable                   (for_)
import           UnliftIO.Async


main :: IO ()
main = do
  queue <- newTBMQueueIO 4
  queue_resp <- newTBMQueueIO 4

  -- Request status every second.
  fillQueueThread queue 1000000

  void $
    -- Drain request queue indefinitely.
    withAsync (drainRequestQueue queue queue_resp) $ \req_t ->
    -- Drain response queue indefinitely.
    withAsync (drainResponseQueue queue_resp) $ \resp_t ->
    atomically $ waitSTM req_t <|> waitSTM resp_t


-- | Enqueue queries forever.
fillQueueThread:: TBMQueue (ClientM StationStatusResponse) -> Int -> IO ()
fillQueueThread queue interval = void $ forkIO $ forever $ do
  threadDelay interval
  fillQueue queue


-- | Enqueue one query.
fillQueue:: TBMQueue (ClientM StationStatusResponse) -> IO ()
fillQueue queue = do
  let queries = [stationStatus]
  for_ queries $ \query ->
    atomically $ writeTBMQueue queue query


-- | Drain request queue forever.
drainRequestQueueThread :: TBMQueue (ClientM StationStatusResponse) -> TBMQueue StationStatusResponse -> IO ()
drainRequestQueueThread queue queue_resp = void $ forkIO $ forever $ do
  putStrLn "Draining request queue indefinitely"
  drainRequestQueue queue queue_resp


-- | Drain request queue once.
drainRequestQueue :: TBMQueue (ClientM StationStatusResponse) -> TBMQueue StationStatusResponse -> IO ()
drainRequestQueue queue queue_resp =
    loop
  where
    loop = do
      mnext <- atomically $ readTBMQueue queue

      case mnext of
        Nothing -> atomically retry
        Just query -> do
          response <- runQueryWithEnv query
          case response of
            Left  err  -> putStrLn $ "Error: " ++ show err
            Right result -> do
              putStrLn "Got response"

              -- Enqueue the response.
              atomically $ writeTBMQueue queue_resp result

              -- Restart loop
              loop


-- | Drain response queue indefinitely.
drainResponseQueueThread :: TBMQueue StationStatusResponse -> IO ()
drainResponseQueueThread queue = void $ forkIO $ forever $ do
  putStrLn "Draining response queue indefinitely"
  drainResponseQueue queue


-- | Drain response queue once.
drainResponseQueue :: TBMQueue StationStatusResponse -> IO ()
drainResponseQueue queue =
    loop
  where
    loop = do
      mnext <- atomically $ readTBMQueue queue

      case mnext of
        Nothing -> atomically retry
        Just response -> do
          time  <- localToSystem $ response_last_updated response
          putStrLn $ "Last updated " ++ show time
          putStrLn $ "TTL: " ++ show (response_ttl response)

          -- Restart loop
          loop
