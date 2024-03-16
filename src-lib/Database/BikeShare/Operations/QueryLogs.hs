{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |

module Database.BikeShare.Operations.QueryLogs
     ( decodeJsonError
     , decodeJsonErrors
     , insertQueryLog
     , latestQueryErrorsE
     , latestQueryErrorsQ
     , queryErrorsE
     ) where

import           AppEnv

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Aeson

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.BeamConvertable
import           Database.BikeShare.DaysAgo
import           Database.BikeShare.Tables.QueryLogs

import           Prelude                                  hiding ( log )


insertQueryLog :: (WithAppMEnv (Env env) Message m, BeamConvertable QueryResult (QueryLogT (QExpr Postgres s))) => QueryResult -> m [QueryLog]
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
