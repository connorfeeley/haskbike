-- |

module Database.BikeShare.StatusVariationQuery
     ( StatusThreshold (..)
     , StatusVariationQuery (..)
     , filterFor_
     , stationIdCondition
     , thresholdCondition
     ) where

import           Control.Lens                                 hiding ( reuse, (<.) )

import           Data.Int                                     ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus


-- | Data type representing a query for station status.
data StatusVariationQuery where
  StatusVariationQuery :: { _status_query_station_id    :: Maybe Int32
                          , _status_query_thresholds    :: [StatusThreshold]
                          } -> StatusVariationQuery
  deriving (Generic, Show, Eq)


-- | Varient representing the type of threshold to apply to the query.
data StatusThreshold where
  EarliestTime  :: UTCTime   -> StatusThreshold
  LatestTime    :: UTCTime   -> StatusThreshold
  deriving (Show, Eq)


-- | Convert a 'StatusQuery' to a fragment of a filter expression.
thresholdCondition :: StatusThreshold -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
thresholdCondition (EarliestTime threshold) status = status ^. statusLastReported >=. val_ threshold
thresholdCondition (LatestTime threshold) status   = status ^. statusLastReported <=. val_ threshold


-- | Construct a filter expression corresponding to the station ID.
stationIdCondition :: Maybe Int32 -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
stationIdCondition (Just stationId) status = _statusStationId status ==. val_ stationId
stationIdCondition Nothing _               = val_ True


-- | Construct a filter expression for a 'StatusQuery'.
filterFor_ :: StatusVariationQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusVariationQuery stationId thresholds) status =
  let thresholdConditions = map (`thresholdCondition` status) thresholds
  in foldr (&&.) (stationIdCondition stationId status) thresholdConditions
