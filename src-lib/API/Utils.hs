-- | Utility functions for the API.

module API.Utils
     ( StationStatusSimple (..)
     , baseStatus
     , mkStatusSimple
     , stationStatusFromSimple
     ) where

import           API.StationStatus

import           Data.Time

import           GHC.Generics      ( Generic )


 {- |
Simple type analogous to 'StationStatus'.

>>> StationStatusSimple 7001 (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 0 0))) 0 0 0 0 0 0 0
 -}
data StationStatusSimple where
  StationStatusSimple :: { stationId       :: Int
                         , lastReported    :: UTCTime
                         , numDocks        :: Int
                         , bikesAvailable  :: Int
                         , bikesDisabled   :: Int
                         , docksAvailable  :: Int
                         , docksDisabled   :: Int
                         , iconicAvailable :: Int
                         , efitAvailable   :: Int
                         , efitG5Available :: Int
                         } -> StationStatusSimple
  deriving (Show, Generic, Eq, Ord)

{-
Smart constructor to create a 'StationStatusSimple' from a 'TimeOfDay' and the number of bikes available/disabled.

>>> mkStatusSimple (TimeOfDay 0 0 0) 0 0 0 1
-}
mkStatusSimple :: TimeOfDay -> Int -> Int -> Int -> Int -> StationStatusSimple
mkStatusSimple time iconic efit efitG5 bikesDisab =
  baseStatusSimple { lastReported    = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime time)
                   , bikesAvailable  = iconic + efit + efitG5
                   , bikesDisabled   = bikesDisab
                   , docksAvailable  = docksAvail (iconic + efit + efitG5 + bikesDisab)
                   , iconicAvailable = iconic
                   , efitAvailable   = efit
                   , efitG5Available = efitG5
                   }
  where
    baseStatusSimple = StationStatusSimple 7001 (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 0 0))) 10 0 0 0 0 0 0 0

    docksAvail bikesAvail' = numDocks baseStatusSimple - bikesAvail'

stationStatusFromSimple :: StationStatus -> StationStatusSimple -> StationStatus
stationStatusFromSimple base status =
  base { _statusStationId             = stationId status
       , _statusLastReported          = Just (lastReported status)
       , _statusNumBikesAvailable     = bikesAvailable status
       , _statusNumBikesDisabled      = bikesDisabled status
       , _statusNumDocksAvailable     = docksAvailable status
       , _statusNumDocksDisabled      = docksDisabled status
       , _statusVehicleDocksAvailable = [mkVehicleDock (docksAvailable status)]
       , _statusVehicleTypesAvailable = availableList
       }
  where
    availableList = [ mkIconic (iconicAvailable status)
                    , mkEfit (efitAvailable status)
                    , mkEfitG5 (efitG5Available status)
                    ]

-- | Station status record initialized with arbitrary defaults.
baseStatus :: StationStatus
baseStatus =
  StationStatus { _statusStationId             = 7001
                , _statusLastReported          = Just (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight))
                , _statusIsChargingStation     = True
                -- Number of bikes available/disabled.
                , _statusNumBikesAvailable     = 0
                , _statusNumBikesDisabled      = 0
                -- Number of docks available/disabled.
                , _statusNumDocksAvailable     = 0
                , _statusNumDocksDisabled      = 0
                -- Number of docks (must match above).
                , _statusVehicleDocksAvailable = [mkVehicleDock 0]
                -- Number of each bike type.
                , _statusVehicleTypesAvailable = [mkIconic 0, mkEfit 0, mkEfitG5 0]
                -- Boring stuff.
                , _statusStatus                = InService
                , _statusIsInstalled           = True
                , _statusIsRenting             = True
                , _statusIsReturning           = True
                , _statusTraffic               = Nothing
                }

-- | Smart constructor for 'VehicleDock'.
mkVehicleDock :: Int -> VehicleDock
mkVehicleDock = VehicleDock [ "ICONIC", "BOOST", "EFIT", "EFIT G5" ]

-- | Smart constructors for 'VehicleType' data.
mkIconic, mkEfit, mkEfitG5 :: Int -> VehicleType
mkIconic = VehicleType Iconic
mkEfit   = VehicleType EFit
mkEfitG5 = VehicleType EFitG5
