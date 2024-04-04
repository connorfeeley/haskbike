{-# LANGUAGE DeriveGeneric #-}

-- | Types for "versions" endpoint.

module Haskbike.API.SystemPricingPlan
     ( SystemPricingPlan (..)
     ) where

import           Data.Aeson
import qualified Data.Text            as T

import           GHC.Generics

import           Haskbike.API.Classes ( HasDataField (..) )


-- | Data type for core response object of 'versions' API.
data SystemPricingPlan where
  SystemPricingPlan :: { sysPricingPlanId          :: T.Text
                       , sysPricingPlanName        :: T.Text
                       , sysPricingPlanCurrency    :: T.Text
                       , sysPricingPlanPrice       :: Double
                       , sysPricingPlanDescription :: T.Text
                       , sysPricingPlanTaxable     :: Bool
                       } -> SystemPricingPlan
  deriving (Show, Eq, Generic)


-- * JSON instances.

instance ToJSON SystemPricingPlan where
  toJSON p =
    object [ "plan_id"     .= sysPricingPlanId           p
           , "name"        .= sysPricingPlanName         p
           , "currency"    .= sysPricingPlanCurrency     p
           , "price"       .= sysPricingPlanPrice        p
           , "description" .= sysPricingPlanDescription  p
           , "is_taxable"  .= sysPricingPlanTaxable      p
           ]
    -- where
    --   -- Convert a Bool to an Int. Only used in v1 API.
    --   intFromBool :: Bool -> Int
    --   intFromBool True  = 1
    --   intFromBool False = 0

instance FromJSON SystemPricingPlan where
  parseJSON = withObject "SystemPricingPlan" $ \v -> SystemPricingPlan
    <$> v .: "plan_id"
    <*> v .: "name"
    <*> v .: "currency"
    <*> v .: "price"
    <*> v .: "description"
    <*> v .: "is_taxable"

instance HasDataField [SystemPricingPlan] where
  -- For a list of SystemPricingPlan, we expect to find them under the 'versions' key
  dataFieldKey = "plans"
