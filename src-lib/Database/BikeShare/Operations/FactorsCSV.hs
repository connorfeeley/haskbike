-- | Types and functions used to calculate availability factors.

module Database.BikeShare.Operations.FactorsCSV
     ( PerformanceDataCSV (..)
     , StatusFactorCSV (..)
     , StatusIntegralCSV (..)
     ) where

import           Data.Csv                              ( DefaultOrdered (..), ToField (..), ToNamedRecord (..),
                                                         ToRecord (..), header, namedRecord, record, (.=) )

import           Database.Beam
import           Database.BikeShare.Operations.Factors

import           Server.Components.PerformanceData


-- | Newtype wrapper used for encoding booleans as a CSV string.
newtype CsvBool = CsvBool Bool
instance ToField CsvBool where
  toField (CsvBool True)  = "TRUE"
  toField (CsvBool False) = "FALSE"


-- | Newtype wrapper used for encoding 'StatusIntegral' as a CSV.
newtype StatusIntegralCSV where
  StatusIntegralCSV :: StatusIntegral -> StatusIntegralCSV
  deriving (Generic, Show, Eq)

-- | Encode 'StatusIntegral' to a CSV.
instance ToRecord StatusIntegralCSV where
    toRecord (StatusIntegralCSV integral) = record
      [ toField (intStatusStationId integral)
      , toField (CsvBool (intStatusCharging integral))
      , toField (intStatusCapacity integral)

      , toField (intStatusSecBikesAvailable integral)
      , toField (intStatusSecBikesDisabled integral)
      , toField (intStatusSecDocksAvailable integral)
      , toField (intStatusSecDocksDisabled integral)

      , toField (intStatusSecIconicAvailable integral)
      , toField (intStatusSecEfitAvailable integral)
      , toField (intStatusSecEfitG5Available integral)
      ]

-- | Encode 'StatusIntegral' by name to a CSV.
instance ToNamedRecord StatusIntegralCSV where
    toNamedRecord (StatusIntegralCSV integral) = namedRecord
      [ "Station ID"                 .= intStatusStationId     integral
      , "Charging"                   .= CsvBool (intStatusCharging integral)
      , "Capacity"                   .= intStatusCapacity      integral
      , "Total Seconds"              .= intStatusTotalSeconds  integral
      , "Bike Available Seconds"     .= intStatusSecBikesAvailable  integral
      , "Bike Disabled Seconds"      .= intStatusSecBikesDisabled   integral
      , "Dock Available Seconds"     .= intStatusSecDocksAvailable  integral
      , "Dock Disabled Seconds"      .= intStatusSecDocksDisabled   integral
      , "Iconic Available Seconds"   .= intStatusSecIconicAvailable integral
      , "E-Fit Available Seconds"    .= intStatusSecEfitAvailable   integral
      , "E-Fit G5 Available Seconds" .= intStatusSecEfitG5Available integral
      ]

-- | Field ordering of 'StatusIntegral' CSV encoding.
instance DefaultOrdered StatusIntegralCSV where
    headerOrder _ = header
      [ "Station ID"
      , "Charging"
      , "Capacity"
      , "Total Seconds"
      , "Bike Available Seconds"
      , "Bike Disabled Seconds"
      , "Dock Available Seconds"
      , "Dock Disabled Seconds"
      , "Iconic Available Seconds"
      , "E-Fit Available Seconds"
      , "E-Fit G5 Available Seconds"
      , "Variation"
      ]

-- | Newtype wrapper used for encoding 'StatusFactor' as a CSV.
newtype StatusFactorCSV where
  StatusFactorCSV :: StatusFactor -> StatusFactorCSV
  deriving (Generic, Show, Eq)

-- | Encode 'StatusIntegral' to a CSV.
instance ToRecord StatusFactorCSV where
    toRecord (StatusFactorCSV factor) = record
      [ toField (statusFactorStationId          factor)
      , toField (CsvBool (statusFactorCharging  factor))
      , toField (statusFactorCapacity           factor)
      , toField (statusFactorTotalSeconds       factor)

      , toField (statusFactorBikesAvailable  factor)
      , toField (statusFactorBikesDisabled   factor)
      , toField (statusFactorDocksAvailable  factor)
      , toField (statusFactorDocksDisabled   factor)

      , toField (statusFactorIconicAvailable factor)
      , toField (statusFactorEfitAvailable   factor)
      , toField (statusFactorEfitG5Available factor)

      , toField (statusFactorNormalizedIconicAvailable factor)
      , toField (statusFactorNormalizedEfitAvailable   factor)
      , toField (statusFactorNormalizedEfitG5Available factor)
      ]

-- | Encode 'StatusFactor' by name to a CSV.
instance ToNamedRecord StatusFactorCSV where
    toNamedRecord (StatusFactorCSV factor) = namedRecord
      [ "Station ID"                           .= statusFactorStationId         factor
      , "Charging"                             .= CsvBool (statusFactorCharging factor)
      , "Capacity"                             .= statusFactorCapacity          factor
      , "Total Seconds"                        .= statusFactorTotalSeconds      factor
      , "Bike Available Factor"                .= statusFactorBikesAvailable    factor
      , "Bike Disabled Factor"                 .= statusFactorBikesDisabled     factor
      , "Dock Available Factor"                .= statusFactorDocksAvailable    factor
      , "Dock Disabled Factor"                 .= statusFactorDocksDisabled     factor
      , "Iconic Available Factor"              .= statusFactorIconicAvailable   factor
      , "E-Fit Available Factor"               .= statusFactorEfitAvailable     factor
      , "E-Fit G5 Available Factor"            .= statusFactorEfitG5Available   factor
      , "Iconic Normalized Available Factor"   .= statusFactorIconicAvailable   factor
      , "E-Fit Normalized Available Factor"    .= statusFactorEfitAvailable     factor
      , "E-Fit G5 Normalized Available Factor" .= statusFactorEfitG5Available   factor
      ]

-- | Field ordering of 'StatusFactor' CSV encoding.
instance DefaultOrdered StatusFactorCSV where
    headerOrder _ = header
      [ "Station ID"
      , "Charging"
      , "Capacity"
      , "Total Seconds"
      , "Bike Available Factor"
      , "Bike Disabled Factor"
      , "Dock Available Factor"
      , "Dock Disabled Factor"
      , "Iconic Available Factor"
      , "E-Fit Available Factor"
      , "E-Fit G5 Available Factor"
      , "Iconic Normalized Available Factor"
      , "E-Fit Normalized Available Factor"
      , "E-Fit G5 Normalized Available Factor"
      ]

-- | Combination of status integrals and factors.
newtype PerformanceDataCSV where
  PerformanceDataCSV :: PerformanceData -> PerformanceDataCSV
  deriving (Generic, Show, Eq)

-- | Encode 'PerformanceData' to a CSV.
instance ToRecord PerformanceDataCSV where
  toRecord (PerformanceDataCSV performanceData) = record
    -- Common info first:
    [ toField (statusFactorStationId          factor)
    , toField (CsvBool (statusFactorCharging  factor))
    , toField (statusFactorCapacity           factor)
    , toField (statusFactorTotalSeconds       factor)

    -- Integral data:
    , toField (intStatusSecBikesAvailable integral)
    , toField (intStatusSecBikesDisabled integral)
    , toField (intStatusSecDocksAvailable integral)
    , toField (intStatusSecDocksDisabled integral)

    , toField (intStatusSecIconicAvailable integral)
    , toField (intStatusSecEfitAvailable integral)
    , toField (intStatusSecEfitG5Available integral)

    -- Corresponding factor data:
    , toField (statusFactorBikesAvailable  factor)
    , toField (statusFactorBikesDisabled   factor)
    , toField (statusFactorDocksAvailable  factor)
    , toField (statusFactorDocksDisabled   factor)

    , toField (statusFactorIconicAvailable factor)
    , toField (statusFactorEfitAvailable   factor)
    , toField (statusFactorEfitG5Available factor)

    , toField (statusFactorNormalizedIconicAvailable factor)
    , toField (statusFactorNormalizedEfitAvailable   factor)
    , toField (statusFactorNormalizedEfitG5Available factor)
    ]
    where
      integral = performanceIntegrals performanceData
      factor = performanceFactors performanceData

-- | Encode 'PerformanceData' by name to a CSV.
instance ToNamedRecord PerformanceDataCSV where
  toNamedRecord (PerformanceDataCSV performanceData) = namedRecord
    [ "Station ID"                           .= statusFactorStationId         factor
    , "Charging"                             .= CsvBool (statusFactorCharging factor)
    , "Capacity"                             .= statusFactorCapacity          factor
    , "Total Seconds"                        .= statusFactorTotalSeconds      factor
    -- Integral data:
    , "Bike Available Seconds"     .= intStatusSecBikesAvailable  integral
    , "Bike Disabled Seconds"      .= intStatusSecBikesDisabled   integral
    , "Dock Available Seconds"     .= intStatusSecDocksAvailable  integral
    , "Dock Disabled Seconds"      .= intStatusSecDocksDisabled   integral
    , "Iconic Available Seconds"   .= intStatusSecIconicAvailable integral
    , "E-Fit Available Seconds"    .= intStatusSecEfitAvailable   integral
    , "E-Fit G5 Available Seconds" .= intStatusSecEfitG5Available integral
    -- Factor data:
    , "Bike Available Factor"                .= statusFactorBikesAvailable    factor
    , "Bike Disabled Factor"                 .= statusFactorBikesDisabled     factor
    , "Dock Available Factor"                .= statusFactorDocksAvailable    factor
    , "Dock Disabled Factor"                 .= statusFactorDocksDisabled     factor
    , "Iconic Available Factor"              .= statusFactorIconicAvailable   factor
    , "E-Fit Available Factor"               .= statusFactorEfitAvailable     factor
    , "E-Fit G5 Available Factor"            .= statusFactorEfitG5Available   factor
    , "Iconic Normalized Available Factor"   .= statusFactorNormalizedIconicAvailable   factor
    , "E-Fit Normalized Available Factor"    .= statusFactorNormalizedEfitAvailable     factor
    , "E-Fit G5 Normalized Available Factor" .= statusFactorNormalizedEfitG5Available   factor
    ]
    where
      integral = performanceIntegrals performanceData
      factor = performanceFactors performanceData

-- | Field ordering of 'PerformanceData' CSV encoding.
instance DefaultOrdered PerformanceDataCSV where
    headerOrder _ = header
      [ "Station ID"
      , "Charging"
      , "Capacity"
      , "Total Seconds"
      , "Bike Available Seconds"
      , "Bike Disabled Seconds"
      , "Dock Available Seconds"
      , "Dock Disabled Seconds"
      , "Bike Available Factor"
      , "Bike Disabled Factor"
      , "Dock Available Factor"
      , "Dock Disabled Factor"
      , "Iconic Available Factor"
      , "E-Fit Available Factor"
      , "E-Fit G5 Available Factor"
      , "Iconic Normalized Available Factor"
      , "E-Fit Normalized Available Factor"
      , "E-Fit G5 Normalized Available Factor"
      ]
