-- | The BikeShare API.

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module BikeShareAPI
  ( BikeShareAPI
  ) where

import           Data.Aeson         (Object)
import           Servant.API

import qualified StationInformation as SI
import qualified StationStatus      as SS


-- | The Bike Share API.
type BikeShareAPI =
  "gbfs_versions"                             :> Get '[JSON] Object
  :<|> "en" :> "vehicle_types"                :> Get '[JSON] Object
  :<|> "en" :> "station_information"          :> Get '[JSON] SI.StationInformationResponse
  :<|> "en" :> "station_status"               :> Get '[JSON] SS.StationStatusResponse
  :<|> "en" :> "system_regions"               :> Get '[JSON] Object
  :<|> "en" :> "system_information"           :> Get '[JSON] Object
  :<|> "en" :> "system_pricing_plans"         :> Get '[JSON] Object
