-- | Types and functions used to calculate availability factors.

module Database.BikeShare.Operations.Factors
     ( StatusFactor (..)
     , StatusIntegral (..)
     , integralToFactor
     , queryIntegratedStatus
     , queryStatusFactors
     , sumBikeStatusFactors
     , sumStatusFactors
     ) where

import           AppEnv

import           Control.Lens                            hiding ( (.=) )
import           Control.Monad.Catch                     ( MonadCatch )

import           Data.Aeson

import           Database.Beam                           hiding ( div_ )
import           Database.BikeShare.Expressions          ( integrateColumns )
import           Database.BikeShare.StatusVariationQuery


-- * Types and functions used to calculate availability integrals.

data StatusIntegral where
  StatusIntegral :: { intStatusStationId       :: Integer
                    , intStatusVariation       :: StatusVariationQuery

                    , intStatusCharging        :: Bool
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

instance ToJSON StatusIntegral where
  toJSON integral =
    object [ "station_id"                .= intStatusStationId     integral

           , "charging"                  .= intStatusCharging      integral
           , "capacity"                  .= intStatusCapacity      integral
           , "total_seconds"             .= intStatusTotalSeconds  integral

           , "bikes_available_seconds"   .= intStatusSecBikesAvailable  integral
           , "bikes_disabled_seconds"    .= intStatusSecBikesDisabled   integral
           , "docks_available_seconds"   .= intStatusSecDocksAvailable  integral
           , "docks_disabled_seconds"    .= intStatusSecDocksDisabled   integral

           , "iconic_available_seconds"  .= intStatusSecIconicAvailable integral
           , "efit_available_seconds"    .= intStatusSecEfitAvailable   integral
           , "efit_g5_available_seconds" .= intStatusSecEfitG5Available integral
           ]

queryIntegratedStatus :: (HasEnv env m, MonadIO m, MonadCatch m) => StatusVariationQuery -> m [StatusIntegral]
queryIntegratedStatus variation = do
  integrals <- withPostgres $ runSelectReturningList $ selectWith $ integrateColumns variation

  pure $
    map (\(sId, charging, capacity, totalSeconds, stationIntegrals, bikeIntegrals) -> (
            StatusIntegral { intStatusVariation          = variation
                           , intStatusStationId          = fromIntegral sId
                           , intStatusCharging           = charging
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


-- * Types and functions used to calculate availability factors.

data StatusFactor where
  StatusFactor :: { statusFactorStationId                 :: Integer
                  , statusFactorVariation                 :: StatusVariationQuery

                  , statusFactorCharging                  :: Bool
                  , statusFactorCapacity                  :: Integer
                  , statusFactorTotalSeconds              :: Integer

                  , statusFactorBikesAvailable            :: Double
                  , statusFactorBikesDisabled             :: Double
                  , statusFactorDocksAvailable            :: Double
                  , statusFactorDocksDisabled             :: Double

                  , statusFactorIconicAvailable           :: Double
                  , statusFactorEfitAvailable             :: Double
                  , statusFactorEfitG5Available           :: Double

                  , statusFactorNormalizedIconicAvailable :: Double
                  , statusFactorNormalizedEfitAvailable   :: Double
                  , statusFactorNormalizedEfitG5Available :: Double
                  } -> StatusFactor
  deriving (Generic, Show, Eq)

instance ToJSON StatusFactor where
  toJSON factor =
    object [ "station_id"               .= statusFactorStationId    factor
           , "charging"                 .= statusFactorCharging     factor
           , "capacity"                 .= statusFactorCapacity     factor
           , "total_seconds"            .= statusFactorTotalSeconds factor

           , "bikes_available_factor"   .= statusFactorBikesAvailable  factor
           , "bikes_disabled_factor"    .= statusFactorBikesDisabled   factor
           , "docks_available_factor"   .= statusFactorDocksAvailable  factor
           , "docks_disabled_factor"    .= statusFactorDocksDisabled   factor

           , "iconic_available_factor"  .= statusFactorIconicAvailable factor
           , "efit_available_factor"    .= statusFactorEfitAvailable   factor
           , "efit_g5_available_factor" .= statusFactorEfitG5Available factor

           , "iconic_available_factor_normalized"  .= statusFactorIconicAvailable factor
           , "efit_available_factor_normalized"    .= statusFactorEfitAvailable   factor
           , "efit_g5_available_factor_normalized" .= statusFactorEfitG5Available factor
           ]

integralToFactor :: StatusIntegral -> StatusFactor
integralToFactor integral =
  StatusFactor { statusFactorVariation       = intStatusVariation    integral
               , statusFactorStationId       = intStatusStationId    integral
               , statusFactorCharging        = intStatusCharging     integral
               , statusFactorCapacity        = intStatusCapacity     integral
               , statusFactorTotalSeconds    = intStatusTotalSeconds integral
               , statusFactorBikesAvailable  = factor intStatusSecBikesAvailable
               , statusFactorBikesDisabled   = factor intStatusSecBikesDisabled
               , statusFactorDocksAvailable  = factor intStatusSecDocksAvailable
               , statusFactorDocksDisabled   = factor intStatusSecDocksDisabled
               , statusFactorIconicAvailable = factor intStatusSecIconicAvailable
               , statusFactorEfitAvailable   = factor intStatusSecEfitAvailable
               , statusFactorEfitG5Available = factor intStatusSecEfitG5Available
               , statusFactorNormalizedIconicAvailable = factor intStatusSecIconicAvailable / availableFactorSum
               , statusFactorNormalizedEfitAvailable   = factor intStatusSecEfitAvailable   / availableFactorSum
               , statusFactorNormalizedEfitG5Available = factor intStatusSecEfitG5Available / availableFactorSum
               }
  where
    totalSeconds = fromInteger (intStatusTotalSeconds integral)
    capacity     = fromInteger (intStatusCapacity     integral)
    factor field = boundFloat (fromInteger (field   integral) / totalSeconds / capacity)
    availableFactorSum = factor intStatusSecIconicAvailable
                       + factor intStatusSecEfitAvailable
                       + factor intStatusSecEfitG5Available

boundFloat :: RealFloat a => a -> a
boundFloat x
  | isInfinite x = 1.0
  | isNaN x = 0.0
  | otherwise = x

queryStatusFactors :: (HasEnv env m, MonadIO m, MonadCatch m) => StatusVariationQuery -> m [StatusFactor]
queryStatusFactors variation = map integralToFactor <$> queryIntegratedStatus variation

sumStatusFactors :: StatusFactor -> Double
sumStatusFactors factors = statusFactorBikesAvailable  factors
                         + statusFactorBikesDisabled   factors
                         + statusFactorDocksAvailable  factors
                         + statusFactorDocksDisabled   factors

sumBikeStatusFactors :: StatusFactor -> Double
sumBikeStatusFactors factors = statusFactorNormalizedIconicAvailable factors
                             + statusFactorNormalizedEfitAvailable   factors
                             + statusFactorNormalizedEfitG5Available factors

