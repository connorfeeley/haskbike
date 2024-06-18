{-# LANGUAGE DerivingStrategies #-}

-- | This module contains definitions regarding user-facing records of endpoint queries.

module Haskbike.Server.Data.QueryHistory
     ( FromRecords (..)
     , QueryHistoryRecord (..)
     , ToLocalTime (..)
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

-- | Typeclass for converting a tuple of database records to a user-facing record.
class ToLocalTime a b where
  toLocalTime :: TimeZone -> a -> b


-- | Record representing the history of queries to a specific endpoint.
data QueryHistoryRecord a where
  QueryHistoryRecord ::
    { endpoint          :: EndpointQueried
    , total             :: QueryHistoryDetailRecord a
    , successful        :: QueryHistoryDetailRecord a
    , failed            :: QueryHistoryDetailRecord a
    } -> QueryHistoryRecord a
  deriving (Show)
  deriving stock (Generic)

instance ToJSON a => ToJSON (QueryHistoryRecord a) where
  toJSON record =
    object [ "endpoint"            .= endpoint record
           , "total"               .= total record
           , "successful"          .= successful record
           , "failed"              .= failed record
           ]

instance FromRecords (EndpointQueried, (Int32, Double, Maybe UTCTime), (Int32, Double, Maybe UTCTime), (Int32, Double, Maybe UTCTime)) (QueryHistoryRecord UTCTime) where
  fromRecords (ep, (total, avgTimeTotal, latest), (successful, avgTimeSuccessful, latestSuccess), (failed, avgTimeFailed, latestFail)) = QueryHistoryRecord
    ep
    (QueryHistoryDetailRecord total      (intervalToNominalDiffTime avgTimeTotal)      latest)
    (QueryHistoryDetailRecord successful (intervalToNominalDiffTime avgTimeSuccessful) latestSuccess)
    (QueryHistoryDetailRecord failed     (intervalToNominalDiffTime avgTimeFailed)     latestFail)
    where
      intervalToNominalDiffTime :: Double -> NominalDiffTime
      intervalToNominalDiffTime = secondsToNominalDiffTime . toPico

      toPico :: Double -> Pico
      toPico = fromInteger . roundDouble -- . (*) 1000000000000

instance ToLocalTime (QueryHistoryRecord UTCTime) (QueryHistoryRecord LocalTime) where
  toLocalTime tz (QueryHistoryRecord ep total successful failed) =
    QueryHistoryRecord ep (toLocalTime tz total) (toLocalTime tz successful) (toLocalTime tz failed)


-- | Record representing the details of an endpoint's query history.
data QueryHistoryDetailRecord a where
  QueryHistoryDetailRecord ::
    { count       :: Int32
    , avgInterval :: NominalDiffTime
    , latest      :: Maybe a
    } -> QueryHistoryDetailRecord a
  deriving (Show)
  deriving stock (Generic)

instance ToJSON a => ToJSON (QueryHistoryDetailRecord a) where
  toJSON record =
    object [ "num-queries"  .= count record
           , "avg-interval" .= toTime (avgInterval record)
           , "latest-query" .= latest record
           ]
    where
      toTime = T.pack . formatTime defaultTimeLocale "%d:%H:%M:%S"

instance ToLocalTime (QueryHistoryDetailRecord UTCTime) (QueryHistoryDetailRecord LocalTime) where
  toLocalTime tz (QueryHistoryDetailRecord count avgInterval latest) =
    QueryHistoryDetailRecord count avgInterval (utcToLocalTime tz <$> latest)
