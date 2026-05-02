-- | Types and functions used to calculate availability factors.

module Haskbike.Database.Operations.Factors
     ( StatusFactor (..)
     , StatusIntegral (..)
     , integralToFactor
     , queryIntegratedStatus
     , queryStatusFactors
     , sumBikeStatusFactors
     , sumStatusFactors
     ) where

import           Control.Lens                           hiding ( (.=) )
import           Control.Monad.Catch                    ( MonadCatch )

import           Data.Aeson

import           Database.Beam                          hiding ( div_ )

import           Haskbike.AppEnv
import           Haskbike.Database.Expressions          ( integrateColumns )
import           Haskbike.Database.StatusVariationQuery


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

                    , intStatusSecBoostAvailable  :: Integer
                    , intStatusSecIconicAvailable :: Integer
                    , intStatusSecEfitAvailable   :: Integer
                    , intStatusSecEfitG5Available :: Integer
                    , intStatusSecChloeAvailable  :: Integer
                    , intStatusSecCosmoAvailable  :: Integer
                    , intStatusSecAstroAvailable  :: Integer
                    , intStatusSecMetroAvailable  :: Integer
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

           , "boost_available_seconds"   .= intStatusSecBoostAvailable  integral
           , "iconic_available_seconds"  .= intStatusSecIconicAvailable integral
           , "efit_available_seconds"    .= intStatusSecEfitAvailable   integral
           , "efit_g5_available_seconds" .= intStatusSecEfitG5Available integral
           , "chloe_available_seconds"   .= intStatusSecChloeAvailable  integral
           , "cosmo_available_seconds"   .= intStatusSecCosmoAvailable  integral
           , "astro_available_seconds"   .= intStatusSecAstroAvailable  integral
           , "metro_available_seconds"   .= intStatusSecMetroAvailable  integral
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
                           , intStatusSecBoostAvailable  = bikeIntegrals ^. _1 & fromIntegral
                           , intStatusSecIconicAvailable = bikeIntegrals ^. _2 & fromIntegral
                           , intStatusSecEfitAvailable   = bikeIntegrals ^. _3 & fromIntegral
                           , intStatusSecEfitG5Available = bikeIntegrals ^. _4 & fromIntegral
                           , intStatusSecChloeAvailable  = bikeIntegrals ^. _5 & fromIntegral
                           , intStatusSecCosmoAvailable  = bikeIntegrals ^. _6 & fromIntegral
                           , intStatusSecAstroAvailable  = bikeIntegrals ^. _7 & fromIntegral
                           , intStatusSecMetroAvailable  = bikeIntegrals ^. _8 & fromIntegral
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

                  , statusFactorBoostAvailable            :: Double
                  , statusFactorIconicAvailable           :: Double
                  , statusFactorEfitAvailable             :: Double
                  , statusFactorEfitG5Available           :: Double
                  , statusFactorChloeAvailable            :: Double
                  , statusFactorCosmoAvailable            :: Double
                  , statusFactorAstroAvailable            :: Double
                  , statusFactorMetroAvailable            :: Double

                  , statusFactorNormalizedBoostAvailable  :: Double
                  , statusFactorNormalizedIconicAvailable :: Double
                  , statusFactorNormalizedEfitAvailable   :: Double
                  , statusFactorNormalizedEfitG5Available :: Double
                  , statusFactorNormalizedChloeAvailable  :: Double
                  , statusFactorNormalizedCosmoAvailable  :: Double
                  , statusFactorNormalizedAstroAvailable  :: Double
                  , statusFactorNormalizedMetroAvailable  :: Double
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

           , "boost_available_factor"   .= statusFactorBoostAvailable  factor
           , "iconic_available_factor"  .= statusFactorIconicAvailable factor
           , "efit_available_factor"    .= statusFactorEfitAvailable   factor
           , "efit_g5_available_factor" .= statusFactorEfitG5Available factor
           , "chloe_available_factor"   .= statusFactorChloeAvailable  factor
           , "cosmo_available_factor"   .= statusFactorCosmoAvailable  factor
           , "astro_available_factor"   .= statusFactorAstroAvailable  factor
           , "metro_available_factor"   .= statusFactorMetroAvailable  factor

           , "boost_available_factor_normalized"   .= statusFactorNormalizedBoostAvailable  factor
           , "iconic_available_factor_normalized"  .= statusFactorNormalizedIconicAvailable factor
           , "efit_available_factor_normalized"    .= statusFactorNormalizedEfitAvailable   factor
           , "efit_g5_available_factor_normalized" .= statusFactorNormalizedEfitG5Available factor
           , "chloe_available_factor_normalized"   .= statusFactorNormalizedChloeAvailable  factor
           , "cosmo_available_factor_normalized"   .= statusFactorNormalizedCosmoAvailable  factor
           , "astro_available_factor_normalized"   .= statusFactorNormalizedAstroAvailable  factor
           , "metro_available_factor_normalized"   .= statusFactorNormalizedMetroAvailable  factor
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

               , statusFactorBoostAvailable  = factor intStatusSecBoostAvailable
               , statusFactorIconicAvailable = factor intStatusSecIconicAvailable
               , statusFactorEfitAvailable   = factor intStatusSecEfitAvailable
               , statusFactorEfitG5Available = factor intStatusSecEfitG5Available
               , statusFactorChloeAvailable  = factor intStatusSecChloeAvailable
               , statusFactorCosmoAvailable  = factor intStatusSecCosmoAvailable
               , statusFactorAstroAvailable  = factor intStatusSecAstroAvailable
               , statusFactorMetroAvailable  = factor intStatusSecMetroAvailable

               , statusFactorNormalizedBoostAvailable  = normalize intStatusSecBoostAvailable
               , statusFactorNormalizedIconicAvailable = normalize intStatusSecIconicAvailable
               , statusFactorNormalizedEfitAvailable   = normalize intStatusSecEfitAvailable
               , statusFactorNormalizedEfitG5Available = normalize intStatusSecEfitG5Available
               , statusFactorNormalizedChloeAvailable  = normalize intStatusSecChloeAvailable
               , statusFactorNormalizedCosmoAvailable  = normalize intStatusSecCosmoAvailable
               , statusFactorNormalizedAstroAvailable  = normalize intStatusSecAstroAvailable
               , statusFactorNormalizedMetroAvailable  = normalize intStatusSecMetroAvailable
               }
  where
    totalSeconds = fromInteger (intStatusTotalSeconds integral)
    capacity     = fromInteger (intStatusCapacity     integral)
    factor field = boundFloat (fromInteger (field   integral) / totalSeconds / capacity)
    normalize field
      | availableFactorSum == 0 = 0
      | otherwise               = factor field / availableFactorSum
    availableFactorSum = factor intStatusSecBoostAvailable
                       + factor intStatusSecIconicAvailable
                       + factor intStatusSecEfitAvailable
                       + factor intStatusSecEfitG5Available
                       + factor intStatusSecChloeAvailable
                       + factor intStatusSecCosmoAvailable
                       + factor intStatusSecAstroAvailable
                       + factor intStatusSecMetroAvailable

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
sumBikeStatusFactors factors = statusFactorNormalizedBoostAvailable  factors
                             + statusFactorNormalizedIconicAvailable factors
                             + statusFactorNormalizedEfitAvailable   factors
                             + statusFactorNormalizedEfitG5Available factors
                             + statusFactorNormalizedChloeAvailable  factors
                             + statusFactorNormalizedCosmoAvailable  factors
                             + statusFactorNormalizedAstroAvailable  factors
                             + statusFactorNormalizedMetroAvailable  factors

