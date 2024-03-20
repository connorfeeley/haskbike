{-# LANGUAGE TemplateHaskell #-}

-- |

module Haskbike.Database.EventCounts
     ( AvailabilityCountVariation (..)
     , ChargingEvent (..)
     , DockingEventsCount (..)
     , EventsCountResult (..)
     , allBikeEvents
     , efitEvents
     , efitG5Events
     , eventsBoostCount
     , eventsCountBikeType
     , eventsCountDockings
     , eventsCountUndockings
     , eventsEfitCount
     , eventsEfitG5Count
     , eventsIconicCount
     , eventsStation
     , eventsVariation
     , iconicEvents
     , sumEvents
     ) where

import           Haskbike.API.VehicleType

import           Control.Lens                                 hiding ( reuse, (.=), (<.) )

import           Data.Aeson

import           Database.Beam
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation


-- | Data type representing the type of statistic to query.
data AvailabilityCountVariation where
  Undocking      :: AvailabilityCountVariation -- ^ Bike undocked (ride began at this station)
  Docking        :: AvailabilityCountVariation -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)


-- | Wrapper for a station and its undocking and docking counts.
data DockingEventsCount where
  DockingEventsCount :: { _eventsStation     :: StationInformation
                        , _eventsVariation   :: StatusVariationQuery
                        , _eventsBoostCount  :: EventsCountResult
                        , _eventsIconicCount :: EventsCountResult
                        , _eventsEfitCount   :: EventsCountResult
                        , _eventsEfitG5Count :: EventsCountResult
                        } -> DockingEventsCount
  deriving (Generic, Show, Eq)

instance ToJSON DockingEventsCount where
  toJSON events =
    object [ "station-id" .= _infoStationId (_eventsStation events)
           , "dockings"   .= object [ "iconic"  .= abs (_eventsCountDockings (_eventsIconicCount events))
                                    , "efit"    .= abs (_eventsCountDockings (_eventsEfitCount   events))
                                    , "efit-g5" .= abs (_eventsCountDockings (_eventsEfitG5Count events))
                                    ]
           , "undockings" .= object [ "iconic"  .= abs (_eventsCountUndockings (_eventsIconicCount events))
                                    , "efit"    .= abs (_eventsCountUndockings (_eventsEfitCount   events))
                                    , "efit-g5" .= abs (_eventsCountUndockings (_eventsEfitG5Count events))
                                    ]
           ]

-- | Wrapper for a station and its undocking and docking counts.
data ChargingEvent where
  ChargingEvent :: { _chargedBikeType     :: TorontoVehicleType
                   , _chargedBikeNumber   :: Int
                   } -> ChargingEvent
  deriving (Generic, Show, Eq)

instance ToJSON ChargingEvent where
  toJSON event =
    object [ "bike-type" .= _chargedBikeType event
           , "count"     .= _chargedBikeNumber event
           ]

-- | Wrapper for the undocking and docking counts for a bike type.
data EventsCountResult =
  EventsCountResult { _eventsCountBikeType   :: TorontoVehicleType
                    , _eventsCountUndockings :: Int
                    , _eventsCountDockings   :: Int
                    }
  deriving (Generic, Show, Eq)


-- | Lenses
makeLenses ''DockingEventsCount
makeLenses ''EventsCountResult


-- | Get events for a specific bike type (all, Iconic, E-Fit, or E-Fit G5).
allBikeEvents, iconicEvents, efitEvents, efitG5Events :: [DockingEventsCount] -> [EventsCountResult]
allBikeEvents ev = iconicEvents ev <> efitEvents ev <> efitG5Events ev
iconicEvents = map _eventsIconicCount
efitEvents   = map _eventsEfitCount
efitG5Events = map _eventsEfitG5Count

{-
Sum bike events for 'Docking' or 'Undocking'.

>>> sumEvents Docking (allBikeEvents ev)
150
-}
sumEvents :: AvailabilityCountVariation -> [EventsCountResult] -> Int
sumEvents Docking   = abs . sum . map _eventsCountDockings
sumEvents Undocking = abs . sum . map _eventsCountUndockings
