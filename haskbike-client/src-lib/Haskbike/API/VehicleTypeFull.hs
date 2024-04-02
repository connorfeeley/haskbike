{-# LANGUAGE DeriveGeneric #-}

-- | Types for "versions" endpoint.

module Haskbike.API.VehicleTypeFull
     ( TorontoVehicleType (..)
     , VehicleTypeFull (..)
     ) where

import           Data.Aeson
import qualified Data.Text                as T

import           GHC.Generics

import           Haskbike.API.Classes     ( HasDataField (..) )
import           Haskbike.API.VehicleType

-- | Data type for core response object of 'versions' API.
data VehicleTypeFull where
  VehicleTypeFull :: { vehicleTypeFullId          :: TorontoVehicleType
                     , vehicleTypeFullFormFactor  :: T.Text
                     , vehicleTypeFullPropulsion  :: T.Text
                     , vehicleTypeFullMaxRange    :: Double -- In meters
                     , vehicleTypeFullName        :: T.Text
                     , vehicleTypeFullPricingPlan :: T.Text -- Default pricing plan string
                     } -> VehicleTypeFull
  deriving (Show, Eq, Generic)


-- * JSON instances.

instance ToJSON VehicleTypeFull  where
  toJSON v =
    object [ "vehicle_type_id"         .= vehicleTypeFullId          v
           , "form_factor"             .= vehicleTypeFullFormFactor  v
           , "propulsion_type"         .= vehicleTypeFullPropulsion  v
           , "max_range_meters"        .= vehicleTypeFullMaxRange    v
           , "name"                    .= vehicleTypeFullName        v
           , "default_pricing_plan_id" .= vehicleTypeFullPricingPlan v
           ]


instance FromJSON VehicleTypeFull where
  parseJSON = withObject "VehicleTypeFull" $ \v -> VehicleTypeFull
    <$> v .: "vehicle_type_id"
    <*> v .: "form_factor"
    <*> v .: "propulsion_type"
    <*> v .: "max_range_meters"
    <*> v .: "name"
    <*> v .: "default_pricing_plan_id"

instance HasDataField [VehicleTypeFull] where
  -- For a list of VehicleTypeFull, we expect to find them under the 'vehicle_types' key
  getDataField obj = obj .: "vehicle_types"
