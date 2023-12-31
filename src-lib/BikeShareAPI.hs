-- | The BikeShare API.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module BikeShareAPI
     ( BikeShareAPI
     ) where

import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus
import           API.SystemInformation

import           Data.Aeson             ( Object )

import           Servant.API


-- | The Bike Share API.
type BikeShareAPI =
  "gbfs_versions"                             :> Get '[JSON] Object
  :<|> "en" :> "vehicle_types"                :> Get '[JSON] Object
  :<|> "en" :> "station_information"          :> Get '[JSON] (ResponseWrapper [StationInformation])
  :<|> "en" :> "station_status"               :> Get '[JSON] (ResponseWrapper [StationStatus])
  :<|> "en" :> "system_regions"               :> Get '[JSON] Object
  :<|> "en" :> "system_information"           :> Get '[JSON] (ResponseWrapper SystemInformation)
  :<|> "en" :> "system_pricing_plans"         :> Get '[JSON] Object
