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


queryLatestSystemInfo :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                      => Maybe LocalTime -> Maybe LocalTime -> m [SystemInformationCount]
queryLatestSystemInfo _startTime _endTime = do
  withPostgres . runSelectReturningList $ selectWith queryLatestSystemInfoE
