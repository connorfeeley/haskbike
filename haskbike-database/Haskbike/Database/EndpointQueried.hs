{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Postgres enum type for the different endpoints that are queried.

module Haskbike.Database.EndpointQueried
     ( EndpointQueried (..)
     , createEndpointQueriedEnum
     , endpointQueriedType
     ) where

import           Control.Applicative                  ( (<|>) )

import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Functor                         ( ($>) )
import qualified Data.Text                            as T

import           Database.Beam
import           Database.Beam.Backend                ( sqlValueSyntax )
import           Database.Beam.Migrate
import           Database.Beam.Postgres               ( Postgres )
import           Database.Beam.Postgres.CustomTypes
import           Database.Beam.Postgres.Syntax        ( PgValueSyntax )
import           Database.PostgreSQL.Simple.FromField

import           Servant.API

data EndpointQueried where
  -- NOTE: update 'queryLatestQueryLogs' if endpoints are added.
  VersionsEP           :: EndpointQueried
  VehicleTypesEP       :: EndpointQueried
  StationInformationEP :: EndpointQueried
  StationStatusEP      :: EndpointQueried
  SystemRegionsEP      :: EndpointQueried
  SystemInformationEP  :: EndpointQueried
  SystemPricingPlansEP :: EndpointQueried
  deriving (Read, Eq, Ord, Enum, Bounded, HasSqlEqualityCheck Postgres)

instance Show EndpointQueried where
  show VersionsEP           = "Versions"
  show VehicleTypesEP       = "Vehicle Types"
  show StationInformationEP = "Station Information"
  show StationStatusEP      = "Station Status"
  show SystemRegionsEP      = "System Regions"
  show SystemInformationEP  = "System Information"
  show SystemPricingPlansEP = "System Pricing Plans"

instance ToJSON EndpointQueried where
  toJSON ep           = String (T.pack (show ep))

instance FromJSON EndpointQueried where
  parseJSON = withText "EndpointQueried" $ \t -> case t of
    "Versions"             -> return VersionsEP
    "Vehicle Types"        -> return VehicleTypesEP
    "Station Information"  -> return StationInformationEP
    "Station Status"       -> return StationStatusEP
    "System Regions"       -> return SystemRegionsEP
    "System Information"   -> return SystemInformationEP
    "System Pricing Plans" -> return SystemPricingPlansEP
    _                      -> fail ("Invalid EndpointQueried: " ++ show t)

instance FromHttpApiData EndpointQueried where
  parseUrlPiece :: T.Text -> Either T.Text EndpointQueried
  parseUrlPiece piece = case parseOnly (
    asciiCI "versions"             $> VersionsEP           <|>
    asciiCI "vehicle-types"        $> VehicleTypesEP       <|>
    asciiCI "station-information"  $> StationInformationEP <|>
    asciiCI "station-status"       $> StationStatusEP      <|>
    asciiCI "system-regions"       $> SystemRegionsEP      <|>
    asciiCI "system-information"   $> SystemInformationEP  <|>
    asciiCI "system-pricing-plans" $> SystemPricingPlansEP
    ) piece of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

instance ToHttpApiData EndpointQueried where
  toQueryParam = toUrlPiece
  toUrlPiece VersionsEP           = T.pack "versions"
  toUrlPiece VehicleTypesEP       = T.pack "vehicle-types"
  toUrlPiece StationInformationEP = T.pack "station-information"
  toUrlPiece StationStatusEP      = T.pack "station-status"
  toUrlPiece SystemRegionsEP      = T.pack "system-regions"
  toUrlPiece SystemInformationEP  = T.pack "system-information"
  toUrlPiece SystemPricingPlansEP = T.pack "system-pricing-plans"


instance FromBackendRow Postgres EndpointQueried where

instance IsPgCustomDataType EndpointQueried where
  -- 'pgDataTypeName' must match the name used in the 'FromField' instance.
  pgDataTypeName _ = "endpoint_queried"

  -- 'pgDataTypeDescription' must also match the implementation of 'fromField' and 'sqlValueSyntax'.
  pgDataTypeDescription :: PgDataTypeSchema EndpointQueried
  pgDataTypeDescription =
    pgCustomEnumSchema
    [VersionsEP, VehicleTypesEP, StationInformationEP, StationStatusEP, SystemRegionsEP, SystemInformationEP, SystemPricingPlansEP]

instance FromField EndpointQueried where
  fromField f mbValue = do
    fieldType <- typename f
    case fieldType of
      "endpoint_queried" -> do
        case mbValue of
          Nothing -> returnError UnexpectedNull f ""
          Just value -> case value of
            "versions"             -> pure VersionsEP
            "vehicle_types"        -> pure VehicleTypesEP
            "station_information"  -> pure StationInformationEP
            "station_status"       -> pure StationStatusEP
            "system_information"   -> pure SystemInformationEP
            "system_regions"       -> pure SystemRegionsEP
            "system_pricing_plans" -> pure SystemPricingPlansEP
            _                      -> returnError ConversionFailed f "Could not 'read' value for 'EndpointQueried'"
      _ -> returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax EndpointQueried where
  sqlValueSyntax = pgEnumValueSyntax $ \case
    VersionsEP           -> "versions"
    VehicleTypesEP       -> "vehicle_types"
    StationInformationEP -> "station_information"
    StationStatusEP      -> "station_status"
    SystemInformationEP  -> "system_information"
    SystemRegionsEP      -> "system_regions"
    SystemPricingPlansEP -> "system_pricing_plans"

-- | Data type of EndpointQueried custom Postgres enum.
endpointQueriedType :: DataType Postgres EndpointQueried
endpointQueriedType = (beamTypeForCustomPg . runMigrationSilenced) createEndpointQueriedEnum

createEndpointQueriedEnum :: Migration Postgres (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
createEndpointQueriedEnum = createEnum "endpoint_queried"
