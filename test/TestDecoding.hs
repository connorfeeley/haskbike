module TestDecoding
     ( unit_stationInformation
     , unit_stationStatus
     , unit_systemInformation
     , unit_systemPricingPlans
     , unit_systemRegions
     , unit_vehicleTypes
     , unit_versions
     ) where

import           API.APIVersion         ( APIVersion )
import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus
import           API.SystemInformation
import           API.SystemPricingPlan  ( SystemPricingPlan )
import           API.SystemRegion       ( SystemRegion )
import           API.VehicleTypeFull    ( VehicleTypeFull )

import           Data.Aeson             ( FromJSON, eitherDecode )
import qualified Data.ByteString        as B
import           Data.ByteString.Lazy   ( fromStrict )
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Maybe             as Maybe

import           Test.Tasty.HUnit

import           Utils

-- import           Data.Aeson.BetterErrors ( Parse, ParseError, ParseError', asIntegral, asString, displayError,
--                                            displayError', eachInArray, eachInObject, key, keyMay, keyOrDefault, nth,
--                                            nthMay, nthOrDefault, parse, toAesonParser, toAesonParser', withString )


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

unit_versions, unit_vehicleTypes, unit_stationInformation, unit_stationStatus, unit_systemInformation, unit_systemRegions, unit_systemPricingPlans :: IO ()

unit_versions           = buildTestCase (undefined :: ResponseWrapper [APIVersion])         "gbfs_versions.json"
unit_vehicleTypes       = buildTestCase (undefined :: ResponseWrapper [VehicleTypeFull])    "vehicle_types.json"
unit_stationInformation = buildTestCase (undefined :: ResponseWrapper [StationInformation]) "station_information.json"
unit_stationStatus      = buildTestCase (undefined :: ResponseWrapper [StationStatus])      "station_status.json"
unit_systemRegions      = buildTestCase (undefined :: ResponseWrapper [SystemRegion])       "system_regions.json"
unit_systemInformation  = buildTestCase (undefined :: ResponseWrapper SystemInformation)    "system_information.json"
unit_systemPricingPlans = buildTestCase (undefined :: ResponseWrapper [SystemPricingPlan])  "system_pricing_plans.json"
