-- |

module Haskbike.Database.StatusVariationQuery
     ( StatusThreshold (..)
     , StatusVariationQuery (..)
     , filterFor_
     , infoFilterForLatest_
     , infoFilterFor_
     , stationIdCondition
     , thresholdCondition
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )

import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres

import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus


-- | Data type representing a query for station status.
data StatusVariationQuery where
  StatusVariationQuery :: { _statusQueryStationId  :: Maybe Int32
                          , _statusQueryThresholds :: [StatusThreshold]
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
stationIdCondition (Just stationId) status = (_unInformationStationId  . _statusInfoId . _statusCommon) status ==. val_ stationId
stationIdCondition Nothing _               = val_ True


-- | Construct a filter expression for a 'StatusQuery'.
filterFor_ :: StatusVariationQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusVariationQuery stationId thresholds) status =
  let thresholdConditions = map (`thresholdCondition` status) thresholds
  in foldr (&&.) (stationIdCondition stationId status) thresholdConditions

-- | Construct a filter expression for a 'StatusQuery'.
infoFilterFor_ :: StatusVariationQuery -> StationInformationT (QExpr Postgres s) -> QExpr Postgres s Bool
infoFilterFor_ (StatusVariationQuery stationId thresholds) status =
  let thresholdConditions = map (`infoThresholdCondition` status) thresholds
  in foldr (&&.) (infoStationIdCondition stationId status) thresholdConditions

-- | Convert a 'StatusQuery' to a fragment of a filter expression.
infoThresholdCondition :: StatusThreshold -> StationInformationT (QExpr Postgres s) -> QExpr Postgres s Bool
infoThresholdCondition (EarliestTime threshold) status = status ^. infoReported >=. val_ threshold
infoThresholdCondition (LatestTime threshold) status   = status ^. infoReported <=. val_ threshold



-- | Construct a filter expression corresponding to the station ID.
infoStationIdCondition :: Maybe Int32 -> StationInformationT (QExpr Postgres s) -> QExpr Postgres s Bool
infoStationIdCondition (Just stationId) status = _infoStationId status ==. val_ stationId
infoStationIdCondition Nothing _               = val_ True

-- | Construct a filter expression for a 'StatusQuery'.
infoFilterForLatest_ :: StatusVariationQuery -> StationInformationT (QExpr Postgres s) -> QExpr Postgres s Bool
infoFilterForLatest_ (StatusVariationQuery stationId thresholds) status =
  let thresholdConditions = map (`infoThresholdConditionLatest` status) thresholds
  in foldr (&&.) (infoStationIdCondition stationId status) thresholdConditions

infoThresholdConditionLatest :: StatusThreshold -> StationInformationT (QExpr Postgres s) -> QExpr Postgres s Bool
infoThresholdConditionLatest (EarliestTime _threshold) _status = val_ True
infoThresholdConditionLatest (LatestTime threshold) status     = status ^. infoReported <=. val_ threshold
