-- | This module contains definitions regarding user-facing records of endpoint queries.

module Haskbike.Server.Data.QueryHistory
     (
     ) where

data QueryHistoryRecord where
  QueryHistoryRecord ::
    { endpoint    :: EndpointQueried
    , total       :: Int32
    , successful  :: Int32
    , failed      :: Int32
    , avgInterval :: DiffTime
    } -> QueryHistoryRecord
  deriving stock (Generic)
