-- |

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Haskbike.Database.Schema.V001.SystemInformation
     ( PrimaryKey (..)
     , SystemInformation
     , SystemInformationCount
     , SystemInformationCountId
     , SystemInformationCountT (..)
     , SystemInformationId
     , SystemInformationKey
     , SystemInformationKeyMixin (..)
     , SystemInformationT (..)
     , createSystemInformation
     , createSystemInformationCount
     , fromBeamSystemInformationToJSON
     , fromJSONToBeamSystemInformation
     , fromJSONToBeamSystemInformationCount
     , sysInfKeyFields
     , sysInfKeyId
     , sysInfKeyReported
     , sysInfoKey
     , systemInformationCountModification
     , systemInformationModification
     ) where

import           Control.Lens

import           Data.Int                       ( Int32 )
import           Data.String                    ( IsString (fromString) )
import qualified Data.Text                      as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend          ( IsSql92DataTypeSyntax (..), SqlSerial )
import           Database.Beam.Migrate
import           Database.Beam.Postgres         ( Postgres )
import qualified Database.Beam.Postgres         as Pg
import           Database.Beam.Postgres.Syntax  ( pgTextType )

import qualified Haskbike.API.SystemInformation as AT


-- | Beam mixin for common system information fields.
data SystemInformationKeyMixin f where
  SystemInformationKey :: { _sysInfKeyId       :: Columnar f (SqlSerial Int32),
                            _sysInfKeyReported :: Columnar f UTCTime
                          } -> SystemInformationKeyMixin f
  deriving (Generic, Beamable)
type SystemInformationKey = SystemInformationKeyMixin Identity
deriving instance Show (SystemInformationKeyMixin Identity)
deriving instance Eq (SystemInformationKeyMixin Identity)

sysInfKeyFields :: (IsString (Columnar f (SqlSerial Int32)), IsString (Columnar f UTCTime)) => String -> SystemInformationKeyMixin f
sysInfKeyFields b =
  SystemInformationKey (fromString (b <> "id"))
                       (fromString (b <> "reported"))

sysInfoKey :: DataType Postgres SystemInformationKey
sysInfoKey = DataType pgTextType


-- | SystemInformationKey Lenses
sysInfKeyId       :: Lens' SystemInformationKey (SqlSerial Int32)
sysInfKeyReported :: Lens' SystemInformationKey UTCTime

SystemInformationKey (LensFor sysInfKeyId)  _      = tableLenses
SystemInformationKey _ (LensFor sysInfKeyReported) = tableLenses


-- | Declare a (Beam) table for the less variable 'SystemInformation' fields.
data SystemInformationT f where
  SystemInformation :: { _sysInfKey                  :: SystemInformationKeyMixin f
                       , _sysInfBuildHash            :: Columnar f T.Text
                       , _sysInfBuildLabel           :: Columnar f T.Text
                       , _sysInfBuildNumber          :: Columnar f T.Text
                       , _sysInfBuildVersion         :: Columnar f T.Text
                       , _sysInfLanguage             :: Columnar f T.Text
                       , _sysInfMobileHeadVersion    :: Columnar f Int32
                       , _sysInfMobileMinSuppVersion :: Columnar f Int32
                       , _sysInfName                 :: Columnar f T.Text
                       , _sysInfSysId                :: Columnar f T.Text
                       , _sysInfTimeZone             :: Columnar f T.Text
                       } -> SystemInformationT f
  deriving (Generic, Beamable)

-- | Declare a (Beam) table for the variable 'SystemInformation' fields.
data SystemInformationCountT f where
  SystemInformationCount :: { _sysInfCntKey             :: SystemInformationKeyMixin f
                            , _sysInfCntStationCount    :: Columnar f Int32
                            , _sysInfCntMechanicalCount :: Columnar f Int32
                            , _sysInfCntEbikeCount      :: Columnar f Int32
                            } -> SystemInformationCountT f
  deriving (Generic, Beamable)

-- * Synonym for the table types.

-- | Synonym for the 'SystemInformation' table type.
type SystemInformation = SystemInformationT Identity
type SystemInformationId = PrimaryKey SystemInformationT Identity
deriving instance Show SystemInformationId
deriving instance Show SystemInformation

-- | Synonym for the 'SystemInformationCount' table type.
type SystemInformationCount = SystemInformationCountT Identity
type SystemInformationCountId = PrimaryKey SystemInformationCountT Identity
deriving instance Show SystemInformationCountId
deriving instance Show SystemInformationCount

-- * Inform Beam about the tables.

-- | Inform Beam about the 'SystemInformation' table.
instance Table SystemInformationT where
  data PrimaryKey SystemInformationT f =
    SystemInformationId { _unSysInfKey :: SystemInformationKeyMixin f }
    deriving (Generic, Beamable)
  primaryKey = SystemInformationId <$> _sysInfKey

-- | Inform Beam about the 'SystemInformationCount' table.
instance Table SystemInformationCountT where
  data PrimaryKey SystemInformationCountT f =
    SystemInformationCountId { _unSysInfCntKey :: SystemInformationKeyMixin f }
    deriving (Generic, Beamable)
  primaryKey = SystemInformationCountId <$> _sysInfCntKey


-- | Convert from the JSON SystemInformation to the Beam SystemInformation type
fromJSONToBeamSystemInformation :: UTCTime -> AT.SystemInformation -> SystemInformationT (QExpr Postgres s)
fromJSONToBeamSystemInformation lastReported inf =
  SystemInformation { _sysInfKey                  = SystemInformationKey default_ (val_ lastReported)
                    , _sysInfBuildHash            = val_ $ AT._sysInfBuildHash    inf
                    , _sysInfBuildLabel           = val_ $ AT._sysInfBuildLabel   inf
                    , _sysInfBuildNumber          = val_ $ AT._sysInfBuildNumber  inf
                    , _sysInfBuildVersion         = val_ $ AT._sysInfBuildVersion inf
                    , _sysInfLanguage             = val_ $ AT._sysInfLanguage inf
                    , _sysInfMobileHeadVersion    = val_ $ fromIntegral $ AT._sysInfMobileHeadVersion    inf
                    , _sysInfMobileMinSuppVersion = val_ $ fromIntegral $ AT._sysInfMobileMinSuppVersion inf
                    , _sysInfName                 = val_ $ AT._sysInfName     inf
                    , _sysInfSysId                = val_ $ AT._sysInfSysId    inf
                    , _sysInfTimeZone             = val_ $ AT._sysInfTimeZone inf
                    }

fromJSONToBeamSystemInformationCount :: forall s. UTCTime -> AT.SystemInformation -> SystemInformationCountT (QExpr Postgres s)
fromJSONToBeamSystemInformationCount lastReported inf =
  SystemInformationCount { _sysInfCntKey             = SystemInformationKey default_ (val_ lastReported)
                         , _sysInfCntStationCount    = (val_ . fromIntegral . AT._sysInfStationCount) inf
                         , _sysInfCntMechanicalCount = (val_ . fromIntegral . AT.sysInfMechanicalCount . AT._sysInfVehicleCount) inf
                         , _sysInfCntEbikeCount      = (val_ . fromIntegral . AT.sysInfEbikeCount      . AT._sysInfVehicleCount) inf
                         }

-- | Convert from the Beam SystemInformation type to the JSON SystemInformation
fromBeamSystemInformationToJSON :: SystemInformation -> SystemInformationCount -> AT.SystemInformation
fromBeamSystemInformationToJSON inf infCnt = AT.SystemInformation
  { AT._sysInfStationCount         = fromIntegral $ _sysInfCntStationCount infCnt
  , AT._sysInfVehicleCount         = AT.SystemInformationVehicleCount (fromIntegral $ _sysInfCntMechanicalCount infCnt) (fromIntegral $ _sysInfCntEbikeCount infCnt)
  , AT._sysInfBuildHash            = _sysInfBuildHash    inf
  , AT._sysInfBuildLabel           = _sysInfBuildLabel   inf
  , AT._sysInfBuildNumber          = _sysInfBuildNumber  inf
  , AT._sysInfBuildVersion         = _sysInfBuildVersion inf
  , AT._sysInfLanguage             = _sysInfLanguage inf
  , AT._sysInfMobileHeadVersion    = fromIntegral $ _sysInfMobileHeadVersion    inf
  , AT._sysInfMobileMinSuppVersion = fromIntegral $ _sysInfMobileMinSuppVersion inf
  , AT._sysInfName                 = _sysInfName     inf
  , AT._sysInfSysId                = _sysInfSysId    inf
  , AT._sysInfTimeZone             = _sysInfTimeZone inf
  }

-- * Table modifications and migrations.

-- | Table modifications for 'SystemInformation' table.
systemInformationModification :: EntityModification (DatabaseEntity be db) be (TableEntity SystemInformationT)
systemInformationModification =
  setEntityName "system_information" <> modifyTableFields tableModification
  { _sysInfKey                   = sysInfKeyFields ""
  , _sysInfBuildHash             = "build_hash"
  , _sysInfBuildLabel            = "build_label"
  , _sysInfBuildNumber           = "build_number"
  , _sysInfBuildVersion          = "build_version"
  , _sysInfLanguage              = "language"
  , _sysInfMobileHeadVersion     = "mobile_head_version"
  , _sysInfMobileMinSuppVersion  = "mobile_minimum_supported_version"
  , _sysInfName                  = "name"
  , _sysInfSysId                 = "system_id"
  , _sysInfTimeZone              = "timezone"
  }

-- | Table modifications for 'SystemInformationCount' table.
systemInformationCountModification :: EntityModification (DatabaseEntity be db) be (TableEntity SystemInformationCountT)
systemInformationCountModification =
  setEntityName "system_information_count" <> modifyTableFields tableModification
  { _sysInfCntKey                = sysInfKeyFields ""
  , _sysInfCntStationCount       = "station_count"
  , _sysInfCntMechanicalCount    = "mechanical_count"
  , _sysInfCntEbikeCount         = "ebike_count"
  }

-- | Migration for the 'SystemInformation' table.
createSystemInformation :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity SystemInformationT))
createSystemInformation =
  createTable "system_information" $ SystemInformation
  { _sysInfKey                  = SystemInformationKey (field "id"         Pg.serial notNull unique)
                                                       (field "reported"   (DataType (timestampType Nothing True)))
  , _sysInfBuildHash            = field "build_hash"                       (DataType pgTextType)
  , _sysInfBuildLabel           = field "build_label"                      (DataType pgTextType)
  , _sysInfBuildNumber          = field "build_number"                     (DataType pgTextType)
  , _sysInfBuildVersion         = field "build_version"                    (DataType pgTextType)
  , _sysInfLanguage             = field "language"                         (DataType pgTextType)
  , _sysInfMobileHeadVersion    = field "mobile_head_version"              int notNull
  , _sysInfMobileMinSuppVersion = field "mobile_minimum_supported_version" int notNull
  , _sysInfName                 = field "name"                             (DataType pgTextType)
  , _sysInfSysId                = field "system_id"                        (DataType pgTextType)
  , _sysInfTimeZone             = field "timezone"                         (DataType pgTextType)
  }

-- | Migration for the 'SystemInformationCount' table.
createSystemInformationCount :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity SystemInformationCountT))
createSystemInformationCount =
  createTable "system_information_count" $ SystemInformationCount
  { _sysInfCntKey             = SystemInformationKey (field "id"       Pg.serial notNull unique)
                                                     (field "reported" (DataType (timestampType Nothing True)))
  , _sysInfCntStationCount    = field "station_count"                  int notNull
  , _sysInfCntMechanicalCount = field "mechanical_count"               int notNull
  , _sysInfCntEbikeCount      = field "ebike_count"                    int notNull
  }
