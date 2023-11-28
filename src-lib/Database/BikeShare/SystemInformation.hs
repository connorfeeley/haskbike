-- |

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare.SystemInformation
     ( SystemInformationCountT (..)
     , SystemInformationKey
     , SystemInformationKeyMixin (..)
     , SystemInformationT (..)
     ) where

import           Control.Lens

import           Data.Int      ( Int32 )
import qualified Data.Text     as T
import           Data.Time

import           Database.Beam


-- | Beam mixin for common system information fields.
data SystemInformationKeyMixin f where
  SystemInformationKey :: { _sysInfKeyId       :: Columnar f Int32,
                            _sysInfKeyReported :: Columnar f UTCTime
                          } -> SystemInformationKeyMixin f
  deriving (Generic, Beamable)
type SystemInformationKey = SystemInformationKeyMixin Identity
deriving instance Show (SystemInformationKeyMixin Identity)
deriving instance Eq (SystemInformationKeyMixin Identity)

-- | SystemInformationKey Lenses
sysInfKeyId       :: Lens' SystemInformationKey Int32
sysInfKeyReported :: Lens' SystemInformationKey UTCTime

SystemInformationKey (LensFor sysInfKeyId)  _      = tableLenses
SystemInformationKey _ (LensFor sysInfKeyReported) = tableLenses


-- | Declare a (Beam) table for the less variable 'SystemInformation' fields.
data SystemInformationT f where
  SystemInformationT :: { _sysInfKey                  :: SystemInformationKeyMixin f
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
  SystemInformationCountT :: { _sysInfCntKey             :: SystemInformationKeyMixin f
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
