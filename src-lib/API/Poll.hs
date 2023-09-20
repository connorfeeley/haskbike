-- |

module API.Poll where

import           API.Client
import           API.ResponseWrapper
import           API.Types                       (StationStatusResponse)
import           Common

import           Servant.Client

import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception               (finally)
import           Control.Monad                   (forever, void)
import           Data.Foldable                   (for_)


main :: IO ()
main = do
  queue <- newTBMQueueIO 4
  queue_resp <- newTBMQueueIO 4

  -- Request status every second
  fillQueueThread queue 1000000

  -- Indefinitely service requests.
  drainRequestQueueThread queue queue_resp

  -- Indefinitely handle responses.
  drainResponseQueue queue_resp

  -- replicateConcurrently_ 8 (drainRequestQueue queue)
  -- concurrently_
  --   (fillQueue queue 1000000 `finally` atomically (closeTBMQueue queue))
  --   (replicateConcurrently_ 8 (drainRequestQueue queue))

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

-- | Drain queue forever.
drainRequestQueueThread :: TBMQueue (ClientM StationStatusResponse) -> TBMQueue StationStatusResponse -> IO ()
drainRequestQueueThread queue queue_resp = void $ forkIO $ forever $ do
  putStrLn "Draining queue"
  drainRequestQueue queue queue_resp

-- | Drain queue once.
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

-- | Drain queue once.
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
