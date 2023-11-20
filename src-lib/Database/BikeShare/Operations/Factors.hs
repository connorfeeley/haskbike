-- | Types and functions used to calculate availability factors.

module Database.BikeShare.Operations.Factors
     ( StatusFactor (..)
     , StatusIntegral (..)
     , queryIntegratedStatus
     , queryStatusFactors
     , sumStatusFactors
     ) where
import           AppEnv

import           Control.Lens

import           Database.Beam
import           Database.BikeShare.Expressions          ( integrateColumns )
import           Database.BikeShare.StationInformation   ( unInformationStationId )
import           Database.BikeShare.StatusVariationQuery


data StatusIntegral where
  StatusIntegral :: { intStatusStationId       :: Integer
                    , intStatusVariation       :: StatusVariationQuery

                    , intStatusCapacity        :: Integer
                    , intStatusTotalSeconds    :: Integer

                    , intStatusSecBikesAvailable  :: Integer
                    , intStatusSecBikesDisabled   :: Integer
                    , intStatusSecDocksAvailable  :: Integer
                    , intStatusSecDocksDisabled   :: Integer

                    , intStatusSecIconicAvailable :: Integer
                    , intStatusSecEfitAvailable   :: Integer
                    , intStatusSecEfitG5Available :: Integer
                    } -> StatusIntegral
  deriving (Generic, Show, Eq)

queryIntegratedStatus :: StatusVariationQuery -> AppM [StatusIntegral]
queryIntegratedStatus variation = do
  integrals <- withPostgres $ runSelectReturningList $ selectWith $ integrateColumns variation

  pure $
    map (\(sId, capacity, totalSeconds, stationIntegrals, bikeIntegrals) -> (
            StatusIntegral { intStatusVariation          = variation
                           , intStatusStationId          = fromIntegral (sId ^. unInformationStationId)
                           , intStatusCapacity           = fromIntegral capacity
                           , intStatusTotalSeconds       = fromIntegral totalSeconds
                           , intStatusSecBikesAvailable  = stationIntegrals ^. _1 & fromIntegral
                           , intStatusSecBikesDisabled   = stationIntegrals ^. _2 & fromIntegral
                           , intStatusSecDocksAvailable  = stationIntegrals ^. _3 & fromIntegral
                           , intStatusSecDocksDisabled   = stationIntegrals ^. _4 & fromIntegral
                           , intStatusSecIconicAvailable = bikeIntegrals ^. _1 & fromIntegral
                           , intStatusSecEfitAvailable   = bikeIntegrals ^. _2 & fromIntegral
                           , intStatusSecEfitG5Available = bikeIntegrals ^. _3 & fromIntegral
                           })
        ) integrals

data StatusFactor where
  StatusFactor :: { statusFactorStationId       :: Integer
                  , statusFactorVariation       :: StatusVariationQuery

                  , statusFactorCapacity        :: Integer

                  , statusFactorBikesAvailable  :: Double
                  , statusFactorBikesDisabled   :: Double
                  , statusFactorDocksAvailable  :: Double
                  , statusFactorDocksDisabled   :: Double

                  , statusFactorIconicAvailable :: Double
                  , statusFactorEfitAvailable   :: Double
                  , statusFactorEfitG5Available :: Double
                  } -> StatusFactor
  deriving (Generic, Show, Eq)

integralToFactor :: StatusIntegral -> StatusFactor
integralToFactor integral =
  StatusFactor { statusFactorVariation       = intStatusVariation integral
               , statusFactorStationId       = intStatusStationId integral
               , statusFactorCapacity        = intStatusCapacity integral
               , statusFactorBikesAvailable  = (fromInteger (intStatusSecBikesAvailable  integral) / totalSeconds) / capacity
               , statusFactorBikesDisabled   = (fromInteger (intStatusSecBikesDisabled   integral) / totalSeconds) / capacity
               , statusFactorDocksAvailable  = (fromInteger (intStatusSecDocksAvailable  integral) / totalSeconds) / capacity
               , statusFactorDocksDisabled   = (fromInteger (intStatusSecDocksDisabled   integral) / totalSeconds) / capacity
               , statusFactorIconicAvailable = (fromInteger (intStatusSecIconicAvailable integral) / totalSeconds) / capacity
               , statusFactorEfitAvailable   = (fromInteger (intStatusSecEfitAvailable   integral) / totalSeconds) / capacity
               , statusFactorEfitG5Available = (fromInteger (intStatusSecEfitG5Available integral) / totalSeconds) / capacity
               }
  where
    totalSeconds = fromInteger (intStatusTotalSeconds integral)
    capacity     = fromInteger (intStatusCapacity     integral)

queryStatusFactors :: StatusVariationQuery -> AppM [StatusFactor]
queryStatusFactors variation = map integralToFactor <$> queryIntegratedStatus variation

sumStatusFactors :: StatusFactor -> Double
sumStatusFactors factors =
    ( statusFactorBikesAvailable  factors
    + statusFactorBikesDisabled   factors
    + statusFactorDocksAvailable  factors
    + statusFactorDocksDisabled   factors
    )
