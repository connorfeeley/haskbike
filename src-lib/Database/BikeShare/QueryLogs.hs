{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Table to hold logs of queries to the API.

module Database.BikeShare.QueryLogs
     ( PrimaryKey (QueryLogId)
     , QueryLog
     , QueryLogId
     , QueryLogT (..)
     , QueryResult (..)
       -- Lenses
     , queryLogEndpoint
     , queryLogErrJson
     , queryLogErrMsg
     , queryLogId
     , queryLogSuccess
     , queryLogTime
     ) where

import           Control.Lens

import           Data.Aeson
import           Data.Int
import qualified Data.Text                          as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend              ( SqlSerial )
import           Database.Beam.Postgres             ( PgJSONB (PgJSONB), Postgres )
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
    QueryLog default_ (val_ t) (val_ ep) (val_ True)  (val_ Nothing) (val_ Nothing)
  convertToBeam (QueryFailure t ep err_msg json_err) =
    QueryLog default_ (val_ t) (val_ ep) (val_ False) ((val_ . Just) err_msg) ((val_ . Just . PgJSONB . toJSON) json_err)

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

