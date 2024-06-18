{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains definitions regarding user-facing records of endpoint queries.

module Haskbike.Server.Data.QueryHistory
     ( FromRecords (..)
     , QueryHistoryRecord (..)
     ) where

import           Data.Aeson
import           Data.Fixed                        ( Pico )
import           Data.Int                          ( Int32 )
import           Data.Time

import           GHC.Float                         ( roundDouble )
import           GHC.Generics                      ( Generic )

import           Haskbike.Database.EndpointQueried


data QueryHistoryRecord where
  QueryHistoryRecord ::
    { endpoint          :: EndpointQueried
    , total             :: Int32
    , avgTimeTotal      :: NominalDiffTime
    , successful        :: Int32
    , avgTimeSuccessful :: NominalDiffTime
    , failed            :: Int32
    , avgTimeFailed     :: NominalDiffTime
    } -> QueryHistoryRecord
  deriving (Show, ToJSON)
  deriving stock (Generic)

class FromRecords a where
  fromRecords :: a -> QueryHistoryRecord

instance FromRecords (EndpointQueried, Int32, Double, Int32, Double, Int32, Double) where
  fromRecords (ep, total, avgTimeTotal, successful, avgTimeSuccessful, failed, avgTimeFailed) =
    QueryHistoryRecord ep
    total (intervalToNominalDiffTime avgTimeTotal)
    successful (intervalToNominalDiffTime avgTimeSuccessful)
    failed (intervalToNominalDiffTime avgTimeFailed)

intervalToNominalDiffTime :: Double -> NominalDiffTime
intervalToNominalDiffTime = secondsToNominalDiffTime . toPico

toPico :: Double -> Pico
toPico = fromInteger . roundDouble . (*) 1000000000000
