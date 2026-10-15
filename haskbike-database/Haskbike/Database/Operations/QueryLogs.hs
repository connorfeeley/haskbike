{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Expressions regarding query logs.

module Haskbike.Database.Operations.QueryLogs
     ( decodeJsonError
     , decodeJsonErrors
     , insertQueryLog
     , latestQueryErrorsE
     , latestQueryErrorsQ
     , markQueryLogReplayedPg
     , queryErrorsE
     , queryReplayableQueryLogs
     , replayableQueryLogsE
     ) where

import           Control.Lens                             hiding ( reuse, (<.) )
import           Control.Monad.Catch                      ( MonadCatch, MonadThrow )

import           Data.Aeson
import           Data.Int                                 ( Int32 )
import           Data.Time                                ( UTCTime )

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Haskbike.AppEnv
import           Haskbike.Database.BeamConvertable
import           Haskbike.Database.BikeShare
import           Haskbike.Database.DaysAgo
import           Haskbike.Database.EndpointQueried
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
    filter_' (\q -> _queryLogSuccess q ==?. val_ False) $
    all_ (_bikeshareQueryLog bikeshareDb)

latestQueryErrorsQ :: DaysAgo -> AppM [QueryLog]
latestQueryErrorsQ days = do
  withPostgres $ runSelectReturningList $ select $ latestQueryErrorsE (val_ days)

{- | Expression selecting query logs that are pending replay: a failed query whose response body
was captured (@error_json@ is present and not the JSON literal @null@) and that has not already
been replayed. Ordered oldest first (@time@, then @id@ to break ties) so that replaying in order
rebuilds station_information history forwards, the same direction the live poller sees it.

Invariant this filter relies on: @error_json@ is only ever non-null for a servant 'DecodeFailure'
whose response body was itself valid, decodable JSON. Tracing from 'Haskbike.CLI.Poll.Utils.persistQueryLog'
through 'Haskbike.CLI.Poll.Utils.maybeDecodeFailure': every other 'ClientError' constructor
(@FailureResponse@, @UnsupportedContentType@, @InvalidContentTypeHeader@, @ConnectionError@) is
logged with a JSON @null@ body unconditionally, and a @DecodeFailure@ whose body does not itself
parse as JSON also falls back to @null@. So "not-null @error_json@" already means "a
'DecodeFailure' with a body we can re-parse and replay" - no extra column comparison is needed to
distinguish failure kinds.

Boundary semantics: @since@ is inclusive (@_queryLogTime >=. since@) and @until@ is inclusive
(@_queryLogTime <=. until@). The optional endpoint/since/until/id arguments each narrow the
result further when supplied.
-}
replayableQueryLogsE :: be ~ Postgres
                     => Maybe EndpointQueried -- ^ Restrict to a single endpoint.
                     -> Maybe UTCTime         -- ^ Restrict to queries logged at or after this time (inclusive).
                     -> Maybe UTCTime         -- ^ Restrict to queries logged at or before this time (inclusive).
                     -> Maybe Int32           -- ^ Restrict to a single query log ID.
                     -> Q be BikeshareDb s (QueryLogT (QGenExpr QValueContext be s))
replayableQueryLogsE endpoint since until_ queryId =
  orderBy_ (\q -> (asc_ (_queryLogTime q), asc_ (_queryLogId q))) $
  filter_' (\q -> _queryLogSuccess q ==?. val_ False &&?.
                  sqlBool_ (isJust_ (_queryLogErrJson q)) &&?.
                  sqlBool_ (isNothing_ (_queryLogReplayedAt q)) &&?.
                  (_queryLogErrJson q /=?. val_ (Just (PgJSONB Null))) &&?.
                  maybe (sqlBool_ (val_ True)) (\ep -> _queryLogEndpoint q ==?. val_ ep)              endpoint &&?.
                  maybe (sqlBool_ (val_ True)) (\t  -> sqlBool_ (_queryLogTime q >=. val_ t))         since &&?.
                  maybe (sqlBool_ (val_ True)) (\t  -> sqlBool_ (_queryLogTime q <=. val_ t))         until_ &&?.
                  maybe (sqlBool_ (val_ True)) (\i  -> _queryLogId q ==?. val_ (SqlSerial i))         queryId
           ) $
  all_ (_bikeshareQueryLog bikeshareDb)

-- | Query logs pending replay, oldest first. See 'replayableQueryLogsE' for the selection
-- invariant and ordering rules. @since@/@until@ are both inclusive bounds on @time@; @limit@ caps
-- the number of rows returned when supplied.
queryReplayableQueryLogs :: (HasEnv env m, MonadIO m, MonadCatch m)
                         => Maybe EndpointQueried -- ^ Restrict to a single endpoint.
                         -> Maybe UTCTime         -- ^ Restrict to queries logged at or after this time (inclusive).
                         -> Maybe UTCTime         -- ^ Restrict to queries logged at or before this time (inclusive).
                         -> Maybe Int32           -- ^ Restrict to a single query log ID.
                         -> Maybe Integer         -- ^ Limit the number of rows returned.
                         -> m [QueryLog]
queryReplayableQueryLogs endpoint since until_ queryId limit =
  case limit of
    Just limit' -> withPostgres $ runSelectReturningList $ select $
                   limit_ limit' (replayableQueryLogsE endpoint since until_ queryId)
    Nothing     -> withPostgres $ runSelectReturningList $ select $
                   replayableQueryLogsE endpoint since until_ queryId

-- | Mark a query log as replayed at the given time.
markQueryLogReplayedPg :: SqlSerial Int32 -> UTCTime -> Pg [QueryLog]
markQueryLogReplayedPg queryId replayedAt =
  runUpdateReturningList $ update (_bikeshareQueryLog bikeshareDb)
    (\q -> _queryLogReplayedAt q <-. val_ (Just replayedAt))
    (\q -> _queryLogId q ==. val_ queryId)

decodeJsonErrors :: (Columnar f1 (Maybe (PgJSONB Value)) ~ f2 (PgJSONB Value),  Functor f3, Functor f2) => f3 (QueryLogT f1) -> f3 (f2 Value)
decodeJsonErrors xs = fmap decodeJsonError . _queryLogErrJson <$> xs

decodeJsonError :: PgJSONB Value -> Value
decodeJsonError (PgJSONB j) = j
