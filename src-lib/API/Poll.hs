-- |

module API.Poll where

import           API.Client
import           API.ResponseWrapper
import           API.Types                       (StationStatusResponse)
import           Common

import           Servant.Client

import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad                   (forever, void)
import           Data.Foldable                   (for_)
import           UnliftIO.Async


main :: IO ()
main = do
  ttl <- newTVarIO (1 * 1000000)
  queue <- newTBMQueueIO 4
  queue_resp <- newTBMQueueIO 4

  -- Request status every second.
  void $ forkIO $ forever $ fillQueue queue ttl

  concurrently_
    -- Drain request queue indefinitely.
    (drainRequestQueue queue queue_resp)
    -- Drain response queue indefinitely.
    (drainResponseQueue queue_resp ttl)


-- | Enqueue one query.
fillQueue:: TBMQueue (ClientM StationStatusResponse) -> TVar Int -> IO ()
fillQueue queue interval_var = do
  -- Wait for the interval to pass.
  interval <- readTVarIO interval_var
  putStrLn $ "FILL: interval=" ++ show (interval `div` 1000000) ++ "s"
  threadDelay interval

  let queries = [stationStatus]
  for_ queries $ \query ->
    atomically $ writeTBMQueue queue query


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
            Left  err  -> putStrLn $ "REQUEST: error: " ++ show err
            Right result -> do
              putStrLn "REQUEST: enqueing response"

              -- Enqueue the response.
              atomically $ writeTBMQueue queue_resp result

              -- Restart loop
              loop


-- | Drain response queue once.
drainResponseQueue :: TBMQueue StationStatusResponse -> TVar Int -> IO ()
drainResponseQueue queue interval_val =
    loop
  where
    loop = do
      mnext <- atomically $ readTBMQueue queue

      case mnext of
        Nothing -> atomically retry
        Just response -> do
          time  <- localToSystem $ response_last_updated response
          putStrLn $ "RESPONSE: TTL=" ++ show (response_ttl response) ++ " | " ++ "last updated=" ++ show time

          -- Update the interval
          -- FIXME: this ends up updating the *next* interval, not the current one.
          atomically $ writeTVar interval_val ((response_ttl response + 1) * 1000000)

          -- Restart loop
          loop
