{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Postgres enum type for the different endpoints that are queried.

module Database.BikeShare.EndpointQueried
     ( EndpointQueried (..)
     , pgTypeEndpoint
     ) where

import           Database.Beam
import           Database.Beam.Backend                      ( sqlValueSyntax )
import           Database.Beam.Postgres                     ( Postgres )
import           Database.Beam.Postgres.CustomTypes
import           Database.Beam.Postgres.Syntax              ( PgDataTypeDescr (..), PgDataTypeSyntax (..),
                                                              PgValueSyntax, emit, pgDataTypeJSON )
import           Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Pg

data EndpointQueried where
  StationInformationEP :: EndpointQueried
  StationStatusEP      :: EndpointQueried
  SystemInformationEP  :: EndpointQueried
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

pgTypeEndpoint :: PgDataTypeSyntax
pgTypeEndpoint = PgDataTypeSyntax (PgDataTypeDescrOid (Pg.typoid Pg.text) Nothing) (emit "ENDPOINT_QUERIED_TYPE") (pgDataTypeJSON "endpoint_queried_type")

instance FromBackendRow Postgres EndpointQueried where

instance FromField EndpointQueried where
  fromField f mbValue = do
    fieldType <- typename f
    case fieldType of
      "endpoint_queried_type" -> do
        case mbValue of
          Nothing -> returnError UnexpectedNull f ""
          Just value -> case value of
            "station_information" -> pure StationInformationEP
            "station_status"      -> pure StationStatusEP
            "system_information"  -> pure SystemInformationEP
            _                     -> returnError ConversionFailed f "Could not 'read' value for 'EndpointQueried'"
      _ ->
        returnError Incompatible f ""

instance HasSqlValueSyntax PgValueSyntax EndpointQueried where
  sqlValueSyntax = pgEnumValueSyntax $ \case
    StationInformationEP -> "station_information"
    StationStatusEP      -> "station_statusB"
    SystemInformationEP  -> "system_information"
