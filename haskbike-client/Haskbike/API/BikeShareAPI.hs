-- | The BikeShare Haskbike.API.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Haskbike.API.BikeShareAPI
     ( BikeShareAPI
     ) where

import           Data.Aeson                      ( Object )

import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation

import           Servant.API


-- | The Bike Share Haskbike.API.
type BikeShareAPI =
  "gbfs_versions"                             :> Get '[JSON] Object
  :<|> "en" :> "vehicle_types"                :> Get '[JSON] Object
  :<|> "en" :> "station_information"          :> Get '[JSON] (ResponseWrapper [StationInformation])
  :<|> "en" :> "station_status"               :> Get '[JSON] (ResponseWrapper [StationStatus])
  :<|> "en" :> "system_regions"               :> Get '[JSON] Object
  :<|> "en" :> "system_information"           :> Get '[JSON] (ResponseWrapper SystemInformation)
  :<|> "en" :> "system_pricing_plans"         :> Get '[JSON] Object
