-- |

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare.SystemInformation where
import           Data.Int      ( Int32 )
import qualified Data.Text     as T
import           Data.Time

import           Database.Beam

-- | Declare a (Beam) table for the less variable 'SystemInformation' fields.
data SystemInformationT f where
  SystemInformationT :: { _sysInfId                   :: Columnar f Int32
                        , _sysInfReported             :: Columnar f UTCTime
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
  SystemInformationCountT :: { _sysInfCntId              :: Columnar f Int32
                             , _sysInfCntReported        :: Columnar f UTCTime
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
    SystemInformationId { _unSysInfId       :: Columnar f Int32
                        , _unSysInfReported :: Columnar f UTCTime
                        }
    deriving (Generic, Beamable)
  primaryKey = SystemInformationId <$> _sysInfId  <*> _sysInfReported

-- | Inform Beam about the 'SystemInformationCount' table.
instance Table SystemInformationCountT where
  data PrimaryKey SystemInformationCountT f =
    SystemInformationCountId { _unSysInfCntId       :: Columnar f Int32
                             , _unSysInfCntReported :: Columnar f UTCTime
                             }
    deriving (Generic, Beamable)
  primaryKey = SystemInformationCountId <$> _sysInfCntId  <*> _sysInfCntReported
