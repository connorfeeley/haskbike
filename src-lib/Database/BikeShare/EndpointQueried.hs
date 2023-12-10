{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Postgres enum type for the different endpoints that are queried.

module Database.BikeShare.EndpointQueried
     ( EndpointQueried (..)
     , createEndpointQueriedEnum
     , endpointQueriedType
     ) where

import           Database.Beam
import           Database.Beam.Backend                ( sqlValueSyntax )
import           Database.Beam.Migrate
import           Database.Beam.Postgres               ( Postgres )
import           Database.Beam.Postgres.CustomTypes
import           Database.Beam.Postgres.Syntax        ( PgValueSyntax )
import           Database.PostgreSQL.Simple.FromField

data EndpointQueried where
  StationInformationEP :: EndpointQueried
  StationStatusEP      :: EndpointQueried
  SystemInformationEP  :: EndpointQueried
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromBackendRow Postgres EndpointQueried where

instance IsPgCustomDataType EndpointQueried where
  -- 'pgDataTypeName' must match the name used in the 'FromField' instance.
  pgDataTypeName _ = "endpoint_queried"

  -- 'pgDataTypeDescription' must also match the implementation of 'fromField' and 'sqlValueSyntax'.
  pgDataTypeDescription :: PgDataTypeSchema EndpointQueried
  pgDataTypeDescription = pgCustomEnumSchema [StationInformationEP, StationStatusEP, SystemInformationEP]

instance FromField EndpointQueried where
  fromField f mbValue = do
    fieldType <- typename f
    case fieldType of
      "endpoint_queried" -> do
        case mbValue of
          Nothing -> returnError UnexpectedNull f ""
          Just value -> case value of
            "station_information" -> pure StationInformationEP
            "station_status"      -> pure StationStatusEP
            "system_information"  -> pure SystemInformationEP
            _                     -> returnError ConversionFailed f "Could not 'read' value for 'EndpointQueried'"
      _ -> returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax EndpointQueried where
  sqlValueSyntax = pgEnumValueSyntax $ \case
    StationInformationEP -> "station_information"
    StationStatusEP      -> "station_status"
    SystemInformationEP  -> "system_information"

-- | Data type of EndpointQueried custom Postgres enum.
endpointQueriedType :: DataType Postgres EndpointQueried
endpointQueriedType = (beamTypeForCustomPg . runMigrationSilenced) createEndpointQueriedEnum

createEndpointQueriedEnum :: Migration Postgres (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
createEndpointQueriedEnum = createEnum "endpoint_queried"
