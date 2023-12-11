{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Table to hold logs of queries to the API.

module Database.BikeShare.Tables.QueryLogs
     ( PrimaryKey (QueryLogId)
     , QueryLog
     , QueryLogId
     , QueryLogT (..)
     , QueryResult (..)
     , createQueries
     , queryLogEndpoint
     , queryLogErrJson
     , queryLogErrMsg
     , queryLogId
     , queryLogModification
     , queryLogSuccess
     , queryLogTime
     ) where

import           Control.Lens

import           Data.Aeson
import           Data.Int
import qualified Data.Text                          as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend              ( IsSql92DataTypeSyntax (..), SqlSerial )
import           Database.Beam.Migrate
import           Database.Beam.Postgres             ( PgJSONB (PgJSONB), Postgres )
import qualified Database.Beam.Postgres             as Pg
import           Database.Beam.Postgres.Syntax
import           Database.BikeShare.BeamConvertable
import           Database.BikeShare.EndpointQueried


-- * Associated concrete type.

data QueryResult where
  QuerySuccess :: UTCTime
               -> EndpointQueried
               -> QueryResult
  QueryFailure :: UTCTime
               -> EndpointQueried
               -> T.Text
               -> Value
               -> QueryResult
  deriving (Show, Eq)

instance BeamConvertable QueryResult (QueryLogT (QExpr Postgres s)) where
  convertToBeam (QuerySuccess t ep) =
    QueryLog default_ (val_ t) (val_ ep) (val_ True) (val_ Nothing) (val_ Nothing)
  convertToBeam (QueryFailure t ep err_msg json_err) =
    QueryLog default_ (val_ t) (val_ ep) (val_ False) errTxt errJson
    where
      errTxt = (val_ . Just) err_msg
      errJson = (val_ . Just . PgJSONB . toJSON) json_err

-- * Beam table definition.

data QueryLogT f where
  QueryLog :: { _queryLogId       :: C f (SqlSerial Int32)
              , _queryLogTime     :: C f UTCTime
              , _queryLogEndpoint :: C f EndpointQueried
              , _queryLogSuccess  :: C f Bool
              , _queryLogErrMsg   :: C f (Maybe T.Text)
              , _queryLogErrJson  :: C f (Maybe (PgJSONB Value))
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

queryLogId       :: Lens' (QueryLogT f) (C f (SqlSerial Int32))
queryLogTime     :: Lens' (QueryLogT f) (C f UTCTime)
queryLogEndpoint :: Lens' (QueryLogT f) (C f EndpointQueried)
queryLogSuccess  :: Lens' (QueryLogT f) (C f Bool)
queryLogErrMsg   :: Lens' (QueryLogT f) (C f (Maybe T.Text))
queryLogErrJson  :: Lens' (QueryLogT f) (C f (Maybe (PgJSONB Value)))
QueryLog (LensFor queryLogId)       _ _ _ _ _ = tableLenses
QueryLog _ (LensFor queryLogTime)     _ _ _ _ = tableLenses
QueryLog _ _ (LensFor queryLogEndpoint) _ _ _ = tableLenses
QueryLog _ _ _ (LensFor queryLogSuccess)  _ _ = tableLenses
QueryLog _ _ _ _ (LensFor queryLogErrMsg)   _ = tableLenses
QueryLog _ _ _ _  _ (LensFor queryLogErrJson) = tableLenses


-- * Table modifications and migrations.

-- | Table modifications for 'QueryLog' table.
queryLogModification :: EntityModification (DatabaseEntity be db) be (TableEntity QueryLogT)
queryLogModification =
  setEntityName "queries" <> modifyTableFields tableModification
  { _queryLogId       = "id"
  , _queryLogTime     = "time"
  , _queryLogEndpoint = "endpoint"
  , _queryLogSuccess  = "success"
  , _queryLogErrMsg   = "error_msg"
  , _queryLogErrJson  = "error_json"
  }

-- | Migration for 'QueryLog' table.
createQueries :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity QueryLogT))
createQueries =
  createTable "queries" $ QueryLog
  { _queryLogId       = field "id"         Pg.serial notNull unique
  , _queryLogTime     = field "time"       (DataType (timestampType Nothing True)) notNull
  , _queryLogEndpoint = field "endpoint"   endpointQueriedType notNull -- Using custom enum type.
  , _queryLogSuccess  = field "success"    boolean notNull
  , _queryLogErrMsg   = field "error_msg"  (DataType pgTextType)
  , _queryLogErrJson  = field "error_json" (maybeType Pg.jsonb)
  }
