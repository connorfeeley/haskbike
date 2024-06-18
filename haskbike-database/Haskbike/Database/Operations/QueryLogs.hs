{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |

module Haskbike.Database.Operations.QueryLogs
     ( decodeJsonError
     , decodeJsonErrors
     , insertQueryLog
     , latestQueryErrorsE
     , latestQueryErrorsQ
     , queryErrorsE
     , queryHistoryCountsE
     ) where

import           Control.Lens                             hiding ( reuse, (<.) )
import           Control.Monad.Catch                      ( MonadCatch, MonadThrow )

import           Data.Aeson
import           Data.Int                                 ( Int32 )

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres                   as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.BeamConvertable
import           Haskbike.Database.BikeShare
import           Haskbike.Database.DaysAgo
import           Haskbike.Database.Tables.QueryLogs

import           Prelude                                  hiding ( log )


insertQueryLog :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
               => QueryResult -> m [QueryLog]
insertQueryLog query =
  withPostgres $ runInsertReturningList $
  insert (bikeshareDb ^. bikeshareQueryLog)
  (insertExpressions [convertToBeam query])


latestQueryErrorsE :: be ~ Postgres
                   => QGenExpr QValueContext be _ DaysAgo
                   -> Q be BikeshareDb s (QueryLogT (QGenExpr QValueContext be s))
latestQueryErrorsE days = do
  orderBy_ (desc_ . _queryLogTime) $
    filter_' (\q -> _queryLogSuccess q ==?. val_ False &&?.
                    sqlBool_ (_queryLogTime q >=. daysAgo_ days)
             ) $
    all_ (_bikeshareQueryLog bikeshareDb)

queryErrorsE :: be ~ Postgres
             => Q be BikeshareDb s (QueryLogT (QGenExpr QValueContext be s))
queryErrorsE = do
  orderBy_ (desc_ . _queryLogTime) $
    filter_' (\q -> _queryLogSuccess q ==?. val_ False
             ) $
    all_ (_bikeshareQueryLog bikeshareDb)

latestQueryErrorsQ :: DaysAgo -> AppM [QueryLog]
latestQueryErrorsQ days = do
  withPostgres $ runSelectReturningList $ select $ do
    latestQueryErrorsE (val_ days)

decodeJsonErrors :: (Columnar f1 (Maybe (PgJSONB Value)) ~ f2 (PgJSONB Value),  Functor f3, Functor f2) => f3 (QueryLogT f1) -> f3 (f2 Value)
decodeJsonErrors xs = fmap decodeJsonError <$> (_queryLogErrJson <$> xs)

decodeJsonError :: PgJSONB Value -> Value
decodeJsonError (PgJSONB j) = j

-- | Query the number of queries for each endpoint.
queryHistoryCountsE = do
  aggregate_ (\(row, pTime, pSuccess) -> ( group_ (_queryLogEndpoint row)
                      -- Total query count
                      , as_ @Int32 countAll_
                      -- Average time delta between queries
                      , fromMaybe_ 0.0 $ avg_ $ cast_ (extract_ Pg.epoch_ (_queryLogTime row) - extract_ Pg.epoch_ pTime) double

                      -- Count of successful queries
                      , as_ @Int32 $ countAll_ `filterWhere_'` (_queryLogSuccess row ==?. val_ True)
                      -- Average time delta between successful queries
                      , fromMaybe_ 0.0 (avg_ (cast_ (extract_ Pg.epoch_ (_queryLogTime row) - extract_ Pg.epoch_ pTime) double)
                                        `filterWhere_'` (_queryLogSuccess row ==?. val_ True &&?. pSuccess ==?. val_ True))


                      -- Count of failed queries
                      , as_ @Int32 $ countAll_ `filterWhere_'` (_queryLogSuccess row ==?. val_ False)
                      -- Average time delta between failed queries
                      , fromMaybe_ 0.0 (avg_ (cast_ (extract_ Pg.epoch_ (_queryLogTime row) - extract_ Pg.epoch_ pTime) double)
                                        `filterWhere_'` (_queryLogSuccess row ==?. val_ False &&?. pSuccess ==?. val_ False))
                      )
             ) $
    withWindow_ (\row -> frame_ (partitionBy_ (_queryLogEndpoint row))
                         (orderPartitionBy_ ((asc_ . _queryLogTime) row))
                         noBounds_)
      (\row w -> ( row
                 , lagWithDefault_ (_queryLogTime row) (val_ (1 :: Integer)) (_queryLogTime row) `over_` w
                 , lagWithDefault_ (_queryLogSuccess row) (val_ (1 :: Integer)) (_queryLogSuccess row) `over_` w
                 )) $
      all_ (_bikeshareQueryLog bikeshareDb)
