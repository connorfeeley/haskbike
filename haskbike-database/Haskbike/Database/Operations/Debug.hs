-- | Database operations to serve the debug API.

module Haskbike.Database.Operations.Debug
     ( latestQueryErrors
     , queryErrors
     ) where

import           Control.Monad.Catch                    ( MonadCatch )

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Database.DaysAgo
import           Haskbike.Database.Operations.QueryLogs
import           Haskbike.Database.Tables.QueryLogs


latestQueryErrors :: (MonadCatch m, HasEnv env m)
                  => DaysAgo
                  -> m [QueryLogT Identity]
latestQueryErrors days = do
  withPostgres . runSelectReturningList . select . limit_ 100 $ latestQueryErrorsE (val_ days)


queryErrors :: (MonadCatch m, HasEnv env m)
            => Integer
            -> m [QueryLogT Identity]
queryErrors limit = do
  withPostgres . runSelectReturningList . select . limit_ limit $ queryErrorsE
