{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |

module Database.BikeShare.Operations.QueryLogs
     ( insertQueryLog
     , latestQueryErrorsE
     , latestQueryErrorsQ
     ) where

import           AppEnv

import           Control.Lens                             hiding ( reuse, (<.) )
import           Control.Monad                            ( guard )

import           Data.Aeson
import qualified Data.ByteString                          as B
import           Data.Maybe                               ( isJust, mapMaybe )

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.BeamConvertable
import           Database.BikeShare.Expressions           ( DaysAgo (..), daysAgo_ )
import           Database.BikeShare.Tables.QueryLogs

import           Prelude                                  hiding ( log )


insertQueryLog :: BeamConvertable QueryResult (QueryLogT (QExpr Postgres s)) => QueryResult -> AppM [QueryLog]
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

latestQueryErrorsQ :: DaysAgo -> AppM [QueryLog]
latestQueryErrorsQ days = do
  errors <- withPostgres $ runSelectReturningList $ select $ do
    latestQueryErrorsE (val_ days)

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure errors

-- decodeJsonErrors :: (Columnar f1 (Maybe (PgJSONB Value))  ~ f2 [B.ByteString],  Functor f2, FromJSON a) => QueryLogT f1 -> f2 [Maybe a]

decodeJsonErrors :: (Columnar f1 (Maybe (PgJSONB Value)) ~ f2 (PgJSONB Value),  Functor f3, Functor f2) => f3 (QueryLogT f1) -> f3 (f2 Value)
decodeJsonErrors xs = fmap decodeJsonError <$> (_queryLogErrJson <$> xs)

decodeJsonError :: PgJSONB Value -> Value
decodeJsonError (PgJSONB j) = j
