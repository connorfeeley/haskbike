{-# LANGUAGE DerivingStrategies #-}

-- | This module contains definitions regarding user-facing records of endpoint queries.

module Haskbike.Server.Data.QueryHistory
     (
     ) where

import           Data.Int                          ( Int32 )
import           Data.Time

import           GHC.Generics                      ( Generic )

import           Haskbike.Database.EndpointQueried


data QueryHistoryRecord where
  QueryHistoryRecord ::
    { endpoint    :: EndpointQueried
    , total       :: Int32
    , successful  :: Int32
    , failed      :: Int32
    , avgInterval :: DiffTime
    } -> QueryHistoryRecord
  deriving stock (Generic)
