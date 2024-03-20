-- | The BikeShare API.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module BikeShareAPI
     ( BikeShareAPI
     ) where

import           API.APIVersion
import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus
import           API.SystemInformation
import           API.SystemPricingPlan  ( SystemPricingPlan )
import           API.SystemRegion       ( SystemRegion )
import           API.VehicleTypeFull    ( VehicleTypeFull )

import           Servant.API


-- | The Bike Share API.
type BikeShareAPI =
  "gbfs_versions"                             :> Get '[JSON] (ResponseWrapper [APIVersion])
  :<|> "en" :> "vehicle_types"                :> Get '[JSON] (ResponseWrapper [VehicleTypeFull])
  :<|> "en" :> "station_information"          :> Get '[JSON] (ResponseWrapper [StationInformation])
  :<|> "en" :> "station_status"               :> Get '[JSON] (ResponseWrapper [StationStatus])
  :<|> "en" :> "system_regions"               :> Get '[JSON] (ResponseWrapper [SystemRegion])
  :<|> "en" :> "system_information"           :> Get '[JSON] (ResponseWrapper SystemInformation)
  :<|> "en" :> "system_pricing_plans"         :> Get '[JSON] (ResponseWrapper [SystemPricingPlan])
