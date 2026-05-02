{-# LANGUAGE TemplateHaskell #-}

-- |

module Haskbike.Database.EventCounts
     ( AvailabilityCountVariation (..)
     , ChargingEvent (..)
     , DockingEventsCount (..)
     , EventsCountResult (..)
     , allBikeEvents
     , astroEvents
     , boostEvents
     , chloeEvents
     , cosmoEvents
     , efitEvents
     , efitG5Events
     , eventsAstroCount
     , eventsBoostCount
     , eventsChloeCount
     , eventsCosmoCount
     , eventsCountBikeType
     , eventsCountDockings
     , eventsCountUndockings
     , eventsEfitCount
     , eventsEfitG5Count
     , eventsIconicCount
     , eventsMetroCount
     , eventsStation
     , eventsVariation
     , iconicEvents
     , metroEvents
     , sumEvents
     ) where

import           Control.Lens                                hiding ( reuse, (.=), (<.) )

import           Data.Aeson

import           Database.Beam

import           Haskbike.API.VehicleType
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
                        , _eventsChloeCount  :: EventsCountResult
                        , _eventsCosmoCount  :: EventsCountResult
                        , _eventsAstroCount  :: EventsCountResult
                        , _eventsMetroCount  :: EventsCountResult
                        } -> DockingEventsCount
  deriving (Generic, Show, Eq)

instance ToJSON DockingEventsCount where
  toJSON events =
    object [ "station-id" .= _infoStationId (_eventsStation events)
           , "dockings"   .= object [ "boost"   .= abs (_eventsCountDockings (_eventsBoostCount  events))
                                    , "iconic"  .= abs (_eventsCountDockings (_eventsIconicCount events))
                                    , "efit"    .= abs (_eventsCountDockings (_eventsEfitCount   events))
                                    , "efit-g5" .= abs (_eventsCountDockings (_eventsEfitG5Count events))
                                    , "chloe"   .= abs (_eventsCountDockings (_eventsChloeCount  events))
                                    , "cosmo"   .= abs (_eventsCountDockings (_eventsCosmoCount  events))
                                    , "astro"   .= abs (_eventsCountDockings (_eventsAstroCount  events))
                                    , "metro"   .= abs (_eventsCountDockings (_eventsMetroCount  events))
                                    ]
           , "undockings" .= object [ "boost"   .= abs (_eventsCountUndockings (_eventsBoostCount  events))
                                    , "iconic"  .= abs (_eventsCountUndockings (_eventsIconicCount events))
                                    , "efit"    .= abs (_eventsCountUndockings (_eventsEfitCount   events))
                                    , "efit-g5" .= abs (_eventsCountUndockings (_eventsEfitG5Count events))
                                    , "chloe"   .= abs (_eventsCountUndockings (_eventsChloeCount  events))
                                    , "cosmo"   .= abs (_eventsCountUndockings (_eventsCosmoCount  events))
                                    , "astro"   .= abs (_eventsCountUndockings (_eventsAstroCount  events))
                                    , "metro"   .= abs (_eventsCountUndockings (_eventsMetroCount  events))
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


-- | Get events for a specific bike type (all, or one of the per-type accessors).
allBikeEvents, boostEvents, iconicEvents, efitEvents, efitG5Events, chloeEvents, cosmoEvents, astroEvents, metroEvents :: [DockingEventsCount] -> [EventsCountResult]
allBikeEvents ev = boostEvents ev <> iconicEvents ev <> efitEvents ev <> efitG5Events ev <> chloeEvents ev <> cosmoEvents ev <> astroEvents ev <> metroEvents ev
boostEvents  = map _eventsBoostCount
iconicEvents = map _eventsIconicCount
efitEvents   = map _eventsEfitCount
efitG5Events = map _eventsEfitG5Count
chloeEvents  = map _eventsChloeCount
cosmoEvents  = map _eventsCosmoCount
astroEvents  = map _eventsAstroCount
metroEvents  = map _eventsMetroCount

{-
Sum bike events for 'Docking' or 'Undocking'.

>>> sumEvents Docking (allBikeEvents ev)
150
-}
sumEvents :: AvailabilityCountVariation -> [EventsCountResult] -> Int
sumEvents Docking   = abs . sum . map _eventsCountDockings
sumEvents Undocking = abs . sum . map _eventsCountUndockings
