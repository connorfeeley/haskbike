{-# LANGUAGE DerivingStrategies #-}

-- | This module contains definitions regarding user-facing records of endpoint queries.

module Haskbike.Server.Data.QueryHistory
     ( FromRecords (..)
     , QueryHistoryRecord (..)
     ) where

import           Data.Aeson
import           Data.Fixed                        ( Pico )
import           Data.Int                          ( Int32 )
import qualified Data.Text                         as T
import           Data.Time

import           GHC.Float                         ( roundDouble )
import           GHC.Generics                      ( Generic )

import           Haskbike.Database.EndpointQueried


-- | Typeclass for converting a tuple of database records to a user-facing record.
class FromRecords a b where
  fromRecords :: a -> b


-- | Record representing the history of queries to a specific endpoint.
data QueryHistoryRecord where
  QueryHistoryRecord ::
    { endpoint          :: EndpointQueried
    , total             :: QueryHistoryDetailRecord
    , successful        :: QueryHistoryDetailRecord
    , failed            :: QueryHistoryDetailRecord
    } -> QueryHistoryRecord
  deriving (Show)
  deriving stock (Generic)

instance ToJSON QueryHistoryRecord where
  toJSON record =
    object [ "endpoint"            .= endpoint record
           , "total"               .= total record
           , "successful"          .= successful record
           , "failed"              .= failed record
           ]

instance FromRecords (EndpointQueried, Int32, Double, Int32, Double, Int32, Double) QueryHistoryRecord where
  fromRecords (ep, total, avgTimeTotal, successful, avgTimeSuccessful, failed, avgTimeFailed) =
    QueryHistoryRecord ep
    (QueryHistoryDetailRecord total      (intervalToNominalDiffTime avgTimeTotal))
    (QueryHistoryDetailRecord successful (intervalToNominalDiffTime avgTimeSuccessful))
    (QueryHistoryDetailRecord failed     (intervalToNominalDiffTime avgTimeFailed))
    where
      intervalToNominalDiffTime :: Double -> NominalDiffTime
      intervalToNominalDiffTime = secondsToNominalDiffTime . toPico

      toPico :: Double -> Pico
      toPico = fromInteger . roundDouble -- . (*) 1000000000000


-- | Record representing the details of an endpoint's query history.
data QueryHistoryDetailRecord where
  QueryHistoryDetailRecord ::
    { count             :: Int32
    , avgTime           :: NominalDiffTime
    } -> QueryHistoryDetailRecord
  deriving (Show)
  deriving stock (Generic)

instance ToJSON QueryHistoryDetailRecord where
  toJSON record =
    object [ "num-queries"         .= count record
           , "avg-time"            .= toTime (avgTime record)
           ]
    where
      toTime = T.pack . formatTime defaultTimeLocale "%d:%H:%M:%S"
