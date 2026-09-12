{-# LANGUAGE OverloadedStrings #-}

-- | In-memory TTL cache for the @comp-charging-infrastructure@ endpoint.
--
-- The @queryChargingInfrastructure@ SQL runs in ~600 ms even with the
-- V005 @(station_id, id DESC)@ index because the @WindowAgg + rank()=1@
-- pattern materialises every partition before the filter. The result
-- changes on the order of weeks (charging stations are added /
-- decommissioned slowly), so the request path can serve from an
-- in-memory cache and skip the SQL entirely for most hits.
--
-- Strategy: bucket the requested timestamp to a 5-minute boundary, then
-- key a single-entry @IORef@ on the bucketed time + a wall-clock TTL.
-- Concurrent users hitting "now" within the same 5-minute window all
-- collide on the same bucketed key; only the first request fills the
-- cache, the rest are O(1) IORef reads.
module Haskbike.Server.Cache.ChargingInfra
     ( ChargingInfraCache
     , ChargingInfraCacheEntry (..)
     , chargingInfraBucket
     , chargingInfraTtl
     , lookupOrFillChargingInfra
     , newChargingInfraCache
     ) where

import           Control.Monad.Catch           ( MonadCatch )

import           Data.Int                      ( Int32 )
import           Data.IORef                    ( IORef, newIORef, readIORef, writeIORef )
import           Data.Time                     ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime )
import           Data.Time.Extras              ( floorUtcToBucket )

import           Haskbike.AppEnv               ( HasEnv )
import           Haskbike.Database.Expressions ( queryChargingInfrastructure )

import           UnliftIO                      ( MonadIO (..) )


-- | A cached @queryChargingInfrastructure@ result keyed by the
-- bucketed input timestamp, along with the wall-clock time the entry
-- was filled (for TTL eviction).
data ChargingInfraCacheEntry where
  ChargingInfraCacheEntry ::
    { cieBucketedAt :: !UTCTime           -- ^ bucketed @atTime@ used as the cache key
    , cieCachedAt   :: !UTCTime           -- ^ wall-clock time when this entry was filled
    , cieValue      :: !(Int32, Int32)    -- ^ @(stationCount, dockCount)@
    } -> ChargingInfraCacheEntry
  deriving Show

-- | Single-entry cache. Multi-entry isn't worth it: the realistic
-- request space is dominated by "now" requests, all of which collide
-- on a single bucketed key. Historical timestamps fall through to the
-- database without caching, which is fine since they're rare.
type ChargingInfraCache = IORef (Maybe ChargingInfraCacheEntry)

-- | Bucket size for the cache key. 5 minutes is small enough to be
-- invisible in the UI (charging-station counts change on the order of
-- weeks) and large enough to collapse the per-microsecond @now()@
-- values that the request path generates onto a single key.
chargingInfraBucket :: NominalDiffTime
chargingInfraBucket = 300

-- | Time-to-live for a cache entry. Matches the bucket size: once an
-- entry has lived 5 minutes, the next request refreshes it. Keeping
-- TTL == bucket means a stable cadence of one DB hit per 5-minute
-- bucket per server process, regardless of request volume.
chargingInfraTtl :: NominalDiffTime
chargingInfraTtl = 300

newChargingInfraCache :: MonadIO m => m ChargingInfraCache
newChargingInfraCache = liftIO (newIORef Nothing)

-- | Look up the cached @(stationCount, dockCount)@ pair for the given
-- timestamp; on miss or expired entry, run
-- 'queryChargingInfrastructure' and refresh the cache.
--
-- Returns 'Nothing' iff the underlying SQL query returns 'Nothing'
-- (which only happens for time-ranges before any data exists). The
-- 'Nothing' path is NOT cached so a transient empty result doesn't
-- stick around past the TTL.
--
-- v1 deliberately omits stampede prevention: two concurrent misses
-- both run the SQL query. The window is at most @chargingInfraTtl@
-- and each query is bounded at ~600 ms, so worst-case duplicate work
-- is small and the cache is still ~50x faster than uncached.
lookupOrFillChargingInfra
  :: (HasEnv env m, MonadCatch m, MonadIO m)
  => ChargingInfraCache -> UTCTime -> m (Maybe (Int32, Int32))
lookupOrFillChargingInfra ref atTime = do
  let bucketed = floorUtcToBucket chargingInfraBucket atTime
  now    <- liftIO getCurrentTime
  cached <- liftIO (readIORef ref)
  case cached of
    Just e | isFresh now bucketed e -> pure (Just (cieValue e))
    _ -> do
      result <- queryChargingInfrastructure bucketed
      case result of
        Just v  -> do
          liftIO $ writeIORef ref $ Just ChargingInfraCacheEntry
            { cieBucketedAt = bucketed
            , cieCachedAt   = now
            , cieValue      = v
            }
          pure (Just v)
        Nothing -> pure Nothing
  where
    isFresh now bucketed e =
      cieBucketedAt e == bucketed
        && diffUTCTime now (cieCachedAt e) < chargingInfraTtl
