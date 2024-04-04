{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

-- | The BikeShare API.

module Haskbike.API.BikeShareAPI
     ( BikeShareAPI
     , BikeShareAPIRoutes (..)
     ) where

import           GHC.Generics                    ( Generic )

import           Haskbike.API.APIVersion
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleTypeFull

import           Servant.API


-- | The Bike Share API.
data BikeShareAPIRoutes mode where
  BikeShareAPIRoutes :: { _versions           :: mode :- "gbfs_versions"                :> Get '[JSON] (ResponseWrapper [APIVersion])
                        , _vehicleTypes       :: mode :- "en" :> "vehicle_types"        :> Get '[JSON] (ResponseWrapper [VehicleTypeFull])
                        , _stationInformation :: mode :- "en" :> "station_information"  :> Get '[JSON] (ResponseWrapper [StationInformation])
                        , _stationStatus      :: mode :- "en" :> "station_status"       :> Get '[JSON] (ResponseWrapper [StationStatus])
                        , _systemRegions      :: mode :- "en" :> "system_regions"       :> Get '[JSON] (ResponseWrapper [SystemRegion])
                        , _systemInformation  :: mode :- "en" :> "system_information"   :> Get '[JSON] (ResponseWrapper SystemInformation)
                        , _systemPricingPlans :: mode :- "en" :> "system_pricing_plans" :> Get '[JSON] (ResponseWrapper [SystemPricingPlan])
                        } -> BikeShareAPIRoutes mode
  deriving stock Generic

type BikeShareAPI = NamedRoutes BikeShareAPIRoutes
