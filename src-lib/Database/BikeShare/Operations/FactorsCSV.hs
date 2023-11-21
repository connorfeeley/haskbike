-- | Types and functions used to calculate availability factors.

module Database.BikeShare.Operations.FactorsCSV
     ( StatusFactorCSV (..)
     , StatusIntegralCSV (..)
     ) where

import           Data.Csv                              ( DefaultOrdered (..), ToField (..), ToNamedRecord (..),
                                                         ToRecord (..), header, namedRecord, record, (.=) )

import           Database.Beam
import           Database.BikeShare.Operations.Factors


-- | Newtype wrapper used for encoding booleans as a CSV string.
newtype CsvBool = CsvBool Bool
instance ToField CsvBool where
  toField (CsvBool True)  = "TRUE"
  toField (CsvBool False) = "FALSE"


-- | Newtype wrapper used for encoding 'StatusIntegral' as a CSV.
newtype StatusIntegralCSV = StatusIntegralCSV StatusIntegral
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
newtype StatusFactorCSV = StatusFactorCSV StatusFactor
  deriving (Generic, Show, Eq)

-- | Encode 'StatusIntegral' to a CSV.
instance ToRecord StatusFactorCSV where
    toRecord (StatusFactorCSV factor) = record
      [ toField (statusFactorStationId          factor)
      , toField (CsvBool (statusFactorCharging  factor))
      , toField (statusFactorCapacity           factor)
      -- , toField (statusFactorTotalSeconds       factor)

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

-- (Data.Csv.encodeDefaultOrderedByName . map StatusIntegralCSV) res
-- (Data.Csv.encodeDefaultOrderedByName . map StatusFactorCSV) res
