{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Expressions to fetch the history of queries for each endpoint.

module Haskbike.Database.Operations.QueryHistory
     ( queryHistory
     , queryHistoryE
     ) where

import           Control.Lens                       hiding ( reuse, (<.) )
import           Control.Monad.Catch                ( MonadCatch )

import           Data.Int                           ( Int32 )
import           Data.Time                          ( UTCTime )

import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres             as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.EndpointQueried  ( EndpointQueried )
import           Haskbike.Database.Tables.QueryLogs

import           Prelude                            hiding ( log )

queryHistory :: (MonadCatch m, HasEnv env m)
             => Maybe EndpointQueried
             -> m [(EndpointQueried, (Int32, Double, Maybe UTCTime), (Int32, Double, Maybe UTCTime), (Int32, Double, Maybe UTCTime))]
queryHistory ep = (withPostgres . runSelectReturningList . selectWith) (queryHistoryE ep)

-- | Expression to calculate query statistics for each endpoint.
queryHistoryE :: ( exp ~ QGenExpr
                 , ctx ~ QValueContext
                 , be ~ Postgres
                 , db ~ BikeshareDb
                 , tup ~ (exp ctx be s Int32, exp ctx be s Double, exp ctx be s (Maybe UTCTime))
                 )
              => Maybe EndpointQueried -> With be db (Q be db s ( exp ctx be s EndpointQueried, tup, tup, tup))
queryHistoryE ep = do
  queries <- selecting $ all_ (bikeshareDb ^. bikeshareQueryLog)
  pure $ do
    totals     <- queryHistoryCountsFor ep QueryResultDontCare   (reuse queries)
    successful <- queryHistoryCountsFor ep QueryResultSuccessful (reuse queries)
    failures   <- queryHistoryCountsFor ep QueryResultFailed     (reuse queries)
    guard_' ((totals ^. _1) ==?. (successful ^. _1) &&?. (totals ^. _1) ==?. (failures ^. _1))
    pure ( totals ^. _1 -- Endpoint
         -- Tuples: (count, avg interval, latest query time)
         , (totals     ^. _2, totals     ^. _3, totals     ^. _4)
         , (successful ^. _2, successful ^. _3, successful ^. _4)
         , (failures   ^. _2, failures   ^. _3, failures   ^. _4)
         )

data QueryHistorySuccessFilter where
  QueryResultDontCare   :: QueryHistorySuccessFilter
  QueryResultSuccessful :: QueryHistorySuccessFilter
  QueryResultFailed     :: QueryHistorySuccessFilter

queryHistoryCountsFor :: Maybe EndpointQueried -> QueryHistorySuccessFilter -> _ -> _
queryHistoryCountsFor ep success queries = do
  aggregate_ (\(row, pTime) -> ( group_ (_queryLogEndpoint row)
                      -- Count of failed queries
                      , as_ @Int32 $ countAll_

                      -- Average time delta between failed queries
                      , fromMaybe_ 0.0 (avg_ (cast_ (extract_ Pg.epoch_ (_queryLogTime row) - extract_ Pg.epoch_ pTime) double))

                      -- Latest query time
                      , max_ (_queryLogTime row)
                      )
             ) $
    withWindow_ (\row -> frame_ (partitionBy_ (_queryLogEndpoint row))
                         (orderPartitionBy_ ((asc_ . _queryLogTime) row))
                         noBounds_)
    (\row w -> ( row
               , lagWithDefault_ (_queryLogTime row) (val_ (1 :: Integer)) (_queryLogTime row) `over_` w
               )) $
    filter_' (\q -> filterEndpointCond ep q &&?. filterSuccessCond success q)
    queries
  where
    -- | Construct a filter expression corresponding to if the query was successful or not (or if we don't care).
    filterEndpointCond :: Maybe EndpointQueried -> QueryLogT (QExpr Postgres s) -> QExpr Postgres s SqlBool
    filterEndpointCond Nothing        _q = (sqlBool_ . val_) True
    filterEndpointCond (Just endpoint) q = _queryLogEndpoint q ==?. val_ endpoint

    -- | Construct a filter expression corresponding to if the query was successful or not (or if we don't care).
    filterSuccessCond :: QueryHistorySuccessFilter -> QueryLogT (QExpr Postgres s) -> QExpr Postgres s SqlBool
    filterSuccessCond QueryResultDontCare  _q = (sqlBool_ . val_) True
    filterSuccessCond QueryResultSuccessful q = _queryLogSuccess q ==?. val_ True
    filterSuccessCond QueryResultFailed     q = _queryLogSuccess q ==?. val_ False
