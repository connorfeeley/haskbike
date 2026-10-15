{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | V006 table to hold logs of queries to the API.
--
-- V006 adds a nullable @replayed_at@ column, tracking queries whose failed API
-- responses have since been re-decoded and re-inserted by the replay subcommand.
-- V001's 'Haskbike.Database.Schema.V001.QueryLogs.QueryLogT' describes the table
-- shape as originally applied and must not change; this module describes the
-- post-V006 shape and the 'alterTable' \/ 'addColumn' step that gets there from
-- the V001 shape.

module Haskbike.Database.Schema.V006.QueryLogs
     ( PrimaryKey (QueryLogId)
     , QueryLog
     , QueryLogId
     , QueryLogT (..)
     , QueryResult (..)
     , migrateQueriesAddReplayedAt
     , queryLogEndpoint
     , queryLogErrJson
     , queryLogErrMsg
     , queryLogId
     , queryLogModification
     , queryLogReplayedAt
     , queryLogSuccess
     , queryLogTime
     ) where

import           Control.Lens

import           Data.Aeson
import           Data.Int
import qualified Data.Text                               as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                   ( IsSql92DataTypeSyntax (..), SqlSerial )
import           Database.Beam.Migrate
import           Database.Beam.Postgres                  ( PgJSONB (PgJSONB), Postgres )

import           Haskbike.Database.BeamConvertable
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Schema.V001.QueryLogs ( QueryResult (..) )
import qualified Haskbike.Database.Schema.V001.QueryLogs as V001


-- * Beam table definition.

data QueryLogT f where
  QueryLog :: { _queryLogId         :: C f (SqlSerial Int32)
              , _queryLogTime       :: C f UTCTime
              , _queryLogEndpoint   :: C f EndpointQueried
              , _queryLogSuccess    :: C f Bool
              , _queryLogErrMsg     :: C f (Maybe T.Text)
              , _queryLogErrJson    :: C f (Maybe (PgJSONB Value))
              , _queryLogReplayedAt :: C f (Maybe UTCTime)
              } -> QueryLogT f
  deriving (Generic, Beamable)

type QueryLog = QueryLogT Identity
type QueryLogId = PrimaryKey QueryLogT Identity

deriving instance Show QueryLogId
deriving instance Eq QueryLogId
deriving instance Show QueryLog
deriving instance Eq QueryLog

instance Table QueryLogT where
  data PrimaryKey QueryLogT f = QueryLogId (C f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = QueryLogId . _queryLogId

instance BeamConvertable QueryResult (QueryLogT (QExpr Postgres s)) where
  convertToBeam (QuerySuccess t ep) =
    QueryLog default_ (val_ t) (val_ ep) (val_ True) (val_ Nothing) (val_ Nothing) (val_ Nothing)
  convertToBeam (QueryFailure t ep err_msg json_err) =
    QueryLog default_ (val_ t) (val_ ep) (val_ False) errTxt errJson (val_ Nothing)
    where
      errTxt = (val_ . Just) err_msg
      errJson = (val_ . Just . PgJSONB . toJSON) json_err

queryLogId         :: Lens' (QueryLogT f) (C f (SqlSerial Int32))
queryLogTime       :: Lens' (QueryLogT f) (C f UTCTime)
queryLogEndpoint   :: Lens' (QueryLogT f) (C f EndpointQueried)
queryLogSuccess    :: Lens' (QueryLogT f) (C f Bool)
queryLogErrMsg     :: Lens' (QueryLogT f) (C f (Maybe T.Text))
queryLogErrJson    :: Lens' (QueryLogT f) (C f (Maybe (PgJSONB Value)))
queryLogReplayedAt :: Lens' (QueryLogT f) (C f (Maybe UTCTime))
QueryLog (LensFor queryLogId)         _ _ _ _ _ _ = tableLenses
QueryLog _ (LensFor queryLogTime)       _ _ _ _ _ = tableLenses
QueryLog _ _ (LensFor queryLogEndpoint)   _ _ _ _ = tableLenses
QueryLog _ _ _ (LensFor queryLogSuccess)    _ _ _ = tableLenses
QueryLog _ _ _ _ (LensFor queryLogErrMsg)     _ _ = tableLenses
QueryLog _ _ _ _ _ (LensFor queryLogErrJson)    _ = tableLenses
QueryLog _ _ _ _ _ _ (LensFor queryLogReplayedAt) = tableLenses


-- * Table modifications and migrations.

-- | Table modifications for the V006 'QueryLog' table.
queryLogModification :: EntityModification (DatabaseEntity be db) be (TableEntity QueryLogT)
queryLogModification =
  setEntityName "queries" <> modifyTableFields tableModification
  { _queryLogId         = "id"
  , _queryLogTime       = "time"
  , _queryLogEndpoint   = "endpoint"
  , _queryLogSuccess    = "success"
  , _queryLogErrMsg     = "error_msg"
  , _queryLogErrJson    = "error_json"
  , _queryLogReplayedAt = "replayed_at"
  }

-- | Migration step that adds the nullable @replayed_at@ column to the V001 'queries' table
-- shape via @ALTER TABLE ... ADD COLUMN@, producing the V006 shape.
migrateQueriesAddReplayedAt :: CheckedDatabaseEntity Postgres db (TableEntity V001.QueryLogT)
                            -> Migration Postgres (CheckedDatabaseEntity Postgres db' (TableEntity QueryLogT))
migrateQueriesAddReplayedAt old =
  alterTable old $ \tbl ->
    QueryLog (V001._queryLogId tbl) (V001._queryLogTime tbl) (V001._queryLogEndpoint tbl)
             (V001._queryLogSuccess tbl) (V001._queryLogErrMsg tbl) (V001._queryLogErrJson tbl)
    <$> addColumn (field "replayed_at" (maybeType (DataType (timestampType Nothing True))))
