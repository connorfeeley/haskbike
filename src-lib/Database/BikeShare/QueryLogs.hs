{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Table to hold logs of queries to the API.

module Database.BikeShare.QueryLogs
     ( PrimaryKey (QueryLogId)
     , QueryLog
     , QueryLogId
     , QueryLogT (..)
       -- Lenses
     , queryLogEndpoint
     , queryLogId
     , queryLogTime
     ) where

import           Control.Lens

import           Data.Int
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend              ( SqlSerial )
import           Database.BikeShare.EndpointQueried


data QueryLogT f where
  QueryLog :: { _queryLogId       :: Columnar f (SqlSerial Int32)
              , _queryLogTime     :: Columnar f UTCTime
              , _queryLogEndpoint :: Columnar f EndpointQueried
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
QueryLog (LensFor queryLogId)       _ _ = tableLenses
QueryLog _ (LensFor queryLogTime)     _ = tableLenses
QueryLog _ _ (LensFor queryLogEndpoint) = tableLenses
