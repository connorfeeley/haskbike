-- |

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare.SystemInformation
     ( PrimaryKey (..)
     , SystemInformation
     , SystemInformationCount
     , SystemInformationCountId
     , SystemInformationCountT (..)
     , SystemInformationId
     , SystemInformationKey
     , SystemInformationKeyMixin (..)
     , SystemInformationT (..)
     , sysInfKeyFields
     , sysInfoKey
       -- Lenses
     , fromJSONToBeamSystemInformation
     , fromJSONToBeamSystemInformationCount
     , sysInfKeyId
     , sysInfKeyReported
     ) where

import qualified API.Types                     as AT

import           Control.Lens

import           Data.Int                      ( Int32 )
import           Data.String                   ( IsString (fromString) )
import qualified Data.Text                     as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend         ( SqlSerial )
import           Database.Beam.Postgres        ( Postgres )
import           Database.Beam.Postgres.Syntax ( pgTextType )


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
  SystemInformationKey (fromString (b <> "_boost"))
                       (fromString (b <> "_iconic"))

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


-- | Convert from the JSON StationInformation to the Beam StationInformation type
fromJSONToBeamSystemInformation :: UTCTime -> AT.SystemInformation -> SystemInformationT (QExpr Postgres s)
fromJSONToBeamSystemInformation lastReported inf =
  SystemInformation { _sysInfKey                  = SystemInformationKey default_ (val_ lastReported)
                    , _sysInfBuildHash            = val_ $ T.pack (AT._sysInfBuildHash    inf)
                    , _sysInfBuildLabel           = val_ $ T.pack (AT._sysInfBuildLabel   inf)
                    , _sysInfBuildNumber          = val_ $ T.pack (AT._sysInfBuildNumber  inf)
                    , _sysInfBuildVersion         = val_ $ T.pack (AT._sysInfBuildVersion inf)
                    , _sysInfLanguage             = val_ $ T.pack (AT._sysInfLanguage inf)
                    , _sysInfMobileHeadVersion    = val_ $ fromIntegral $ AT._sysInfMobileHeadVersion    inf
                    , _sysInfMobileMinSuppVersion = val_ $ fromIntegral $ AT._sysInfMobileMinSuppVersion inf
                    , _sysInfName                 = val_ $ T.pack (AT._sysInfName     inf)
                    , _sysInfSysId                = val_ $ T.pack (AT._sysInfSysId    inf)
                    , _sysInfTimeZone             = val_ $ T.pack (AT._sysInfTimeZone inf)
                    }

fromJSONToBeamSystemInformationCount :: UTCTime -> AT.SystemInformation -> SystemInformationCountT (QExpr Postgres s)
fromJSONToBeamSystemInformationCount lastReported inf =
  SystemInformationCount { _sysInfCntKey             = SystemInformationKey default_ (val_ lastReported)
                         , _sysInfCntStationCount    = (val_ . fromIntegral . AT._sysInfStationCount) inf
                         , _sysInfCntMechanicalCount = (val_ . fromIntegral . AT.sysInfMechanicalCount . AT._sysInfVehicleCount) inf
                         , _sysInfCntEbikeCount      = (val_ . fromIntegral . AT.sysInfEbikeCount      . AT._sysInfVehicleCount) inf
                         }
