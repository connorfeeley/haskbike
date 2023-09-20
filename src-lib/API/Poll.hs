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
  queue <- newTBMQueueIO 16
  fillQueueThread queue 1000000
  drainQueue queue
  -- replicateConcurrently_ 8 (drainQueue queue)
  -- concurrently_
  --   (fillQueue queue 1000000 `finally` atomically (closeTBMQueue queue))
  --   (replicateConcurrently_ 8 (drainQueue queue))

fillQueueThread:: TBMQueue (ClientM StationStatusResponse) -> Int -> IO ()
fillQueueThread queue interval = void $ forkIO $ forever $ do
  threadDelay interval
  fillQueue queue

fillQueue:: TBMQueue (ClientM StationStatusResponse) -> IO ()
fillQueue queue = do
  let queries = [stationStatus]
  for_ queries $ \query ->
    atomically $ writeTBMQueue queue query

drainQueueThread :: TBMQueue (ClientM StationStatusResponse) -> IO ()
drainQueueThread queue = void $ forkIO $ forever $ do
  putStrLn "Draining queue"
  drainQueue queue

drainQueue :: TBMQueue (ClientM StationStatusResponse) -> IO ()
drainQueue queue =
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
              time  <- localToSystem $ response_last_updated result
              putStrLn $ "Last updated " ++ show time
              putStrLn $ "TTL: " ++ show (response_ttl result)

              -- Restart loop
              loop
