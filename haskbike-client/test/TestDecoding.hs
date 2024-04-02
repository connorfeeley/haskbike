module TestDecoding
     ( tests
     ) where

import           Data.Aeson                      ( FromJSON, eitherDecode )
import qualified Data.ByteString                 as B
import           Data.ByteString.Lazy            ( fromStrict )
import qualified Data.ByteString.Lazy            as BL
import qualified Data.Maybe                      as Maybe

import           Haskbike.API.APIVersion
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleTypeFull

import           Test.Tasty
import           Test.Tasty.HUnit

import           Utils


-- | Decode a ByteString into a value.
decodeByteString :: FromJSON a => BL.ByteString -> Either String a
decodeByteString = eitherDecode

-- | Test parsing of a ByteString.
testParse :: forall a. FromJSON a => a -> B.ByteString -> IO ()
testParse (_ :: a) bs = do
    result :: Either String a <- return $ decodeByteString $ fromStrict bs
    case result of
        Left err -> assertFailure $ "Error parsing: " ++ err
        Right _  -> return ()

-- | Create a test case.
buildTestCase :: FromJSON a => a -> String -> IO ()
buildTestCase (_ :: a) file = testParse (undefined :: a) (Maybe.fromMaybe "" $ lookupJson file)

-- | Test decoding of JSON files.

tests :: TestTree
tests = testGroup "Decoding tests"
  [ versions
  , vehicleTypes
  , stationInformation
  , stationStatus
  , systemInformation
  , systemRegions
  , systemPricingPlans
  ]

versions, vehicleTypes, stationInformation, stationStatus, systemInformation, systemRegions, systemPricingPlans :: TestTree

versions           = testCase "Decode versions"             (buildTestCase (undefined :: ResponseWrapper [APIVersion])         "gbfs_versions.json")
vehicleTypes       = testCase "Decode vehicle types"        (buildTestCase (undefined :: ResponseWrapper [VehicleTypeFull])    "vehicle_types.json")
stationInformation = testCase "Decode station information"  (buildTestCase (undefined :: ResponseWrapper [StationInformation]) "station_information.json")
stationStatus      = testCase "Decode station status"       (buildTestCase (undefined :: ResponseWrapper [StationStatus])      "station_status.json")
systemRegions      = testCase "Decode system regions"       (buildTestCase (undefined :: ResponseWrapper [SystemRegion])       "system_regions.json")
systemInformation  = testCase "Decode system information"   (buildTestCase (undefined :: ResponseWrapper SystemInformation)    "system_information.json")
systemPricingPlans = testCase "Decode system pricing plans" (buildTestCase (undefined :: ResponseWrapper [SystemPricingPlan])  "system_pricing_plans.json")
