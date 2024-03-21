-- | The BikeShare Haskbike.API.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Haskbike.API.BikeShareAPI
     ( BikeShareAPI
     ) where

import           Haskbike.API.APIVersion
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleTypeFull

import           Servant.API


-- | The Bike Share Haskbike.API.
type BikeShareAPI =
  "gbfs_versions"                             :> Get '[JSON] (ResponseWrapper [APIVersion])
  :<|> "en" :> "vehicle_types"                :> Get '[JSON] (ResponseWrapper [VehicleTypeFull])
  :<|> "en" :> "station_information"          :> Get '[JSON] (ResponseWrapper [StationInformation])
  :<|> "en" :> "station_status"               :> Get '[JSON] (ResponseWrapper [StationStatus])
  :<|> "en" :> "system_regions"               :> Get '[JSON] (ResponseWrapper [SystemRegion])
  :<|> "en" :> "system_information"           :> Get '[JSON] (ResponseWrapper SystemInformation)
  :<|> "en" :> "system_pricing_plans"         :> Get '[JSON] (ResponseWrapper [SystemPricingPlan])
