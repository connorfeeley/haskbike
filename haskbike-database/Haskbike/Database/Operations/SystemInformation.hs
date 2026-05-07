-- | This module contains the database operations for the system information.

module Haskbike.Database.Operations.SystemInformation
     ( queryLatestSystemInfo
     ) where

import           Control.Monad.Catch                        ( MonadCatch )

import           Data.Time

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Database.Expressions
import           Haskbike.Database.Tables.SystemInformation

import           UnliftIO


-- | When both bounds are supplied, runs the time-bounded variant
-- ('queryLatestSystemInfoBetweenE'), which scans only the requested range
-- before deduping unique count tuples; this avoids a full-table scan and
-- a cross-history dedup. When either bound is 'Nothing', falls back to
-- the original all-time query (used by the visualization page, which
-- doesn't pass bounds in its data URL).
queryLatestSystemInfo :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                      => Maybe UTCTime -> Maybe UTCTime -> m [SystemInformationCount]
queryLatestSystemInfo (Just startT) (Just endT) =
  withPostgres . runSelectReturningList . selectWith $
    queryLatestSystemInfoBetweenE startT endT
queryLatestSystemInfo _ _ =
  withPostgres . runSelectReturningList . selectWith $ queryLatestSystemInfoE
