{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Types and functions used to calculate availability factors.

module Database.BikeShare.Operations.Factors
     ( PerformanceData (..)
     , StatusFactor (..)
     , StatusIntegral (..)
     , integralToFactor
     , integralToPerformanceData
     , queryIntegratedStatus
     , queryStatusFactors
     , sumBikeStatusFactors
     , sumStatusFactors
     ) where
import           AppEnv

import           Control.Lens                            hiding ( (.=) )

import           Data.Aeson
import qualified Data.Text                               as T

import           Database.Beam                           hiding ( div_ )
import           Database.BikeShare.Expressions          ( integrateColumns )
import           Database.BikeShare.StationInformation   ( unInformationStationId )
import           Database.BikeShare.StatusVariationQuery

import           Lucid                                   ( HtmlT, Term, ToHtml (..) )
import           Lucid.Html5

import           Server.Page.Utils


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

queryIntegratedStatus :: StatusVariationQuery -> AppM [StatusIntegral]
queryIntegratedStatus variation = do
  integrals <- withPostgres $ runSelectReturningList $ selectWith $ integrateColumns variation

  pure $
    map (\(sId, charging, capacity, totalSeconds, stationIntegrals, bikeIntegrals) -> (
            StatusIntegral { intStatusVariation          = variation
                           , intStatusStationId          = fromIntegral (sId ^. unInformationStationId)
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

queryStatusFactors :: StatusVariationQuery -> AppM [StatusFactor]
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

-- | Combination of status integrals and factors.
data PerformanceData where
  PerformanceData :: { performanceIntegrals :: StatusIntegral, performanceFactors :: StatusFactor }
                 -> PerformanceData
  deriving (Generic, Show, Eq)

integralToPerformanceData :: StatusIntegral -> PerformanceData
integralToPerformanceData integral = PerformanceData integral (integralToFactor integral)

instance ToHtml PerformanceData where
  toHtmlRaw = toHtml
  toHtml params =
    div_ $ do
      div_ [class_ "tooltip"] $ do
        label_ [for_ "performance-data"] (h3_ "Performance")
        div_ [class_ "tooltip-bottom", style_ "min-width: 250px;"] $ do -- Tooltip content
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesAvailable < 0.2)  (factorOf bikesAvailable (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesDisabled  > 0.1)  (factorOf bikesDisabled  (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksAvailable < 0.2)  (factorOf docksAvailable (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksDisabled  > 0.05) (factorOf docksDisabled  (Just 5)))
      div_ [id_ "performance-data"] $ do
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesAvailable < 0.2)  (factorOf bikesAvailable (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesDisabled  > 0.1)  (factorOf bikesDisabled  (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksAvailable < 0.2)  (factorOf docksAvailable (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksDisabled  > 0.05) (factorOf docksDisabled  (Just 2)))

    where
      factorOf :: Monad m => Double -> Maybe Int -> HtmlT m ()
      factorOf factor truncateNum =
        case truncateNum of
          Just decimals -> (toHtml . toPercentage decimals) factor <> "%"
          Nothing       ->  showth factor <> "%"

      getFactor factor = (factor . performanceFactors) params

      bikesAvailable = getFactor statusFactorBikesAvailable
      bikesDisabled  = getFactor statusFactorBikesDisabled
      docksAvailable = getFactor statusFactorDocksAvailable
      docksDisabled  = getFactor statusFactorDocksDisabled

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
  where
    t = 10^n

toPercentage :: Int -> Double -> T.Text
toPercentage decimals factor =
  if percent < 0.05
  then T.pack ("~" ++ show 0)
  else (T.pack . show) percent
  where percent = truncate' (factor * 100) decimals

elemIf :: (t -> t) -> Bool -> t -> t
elemIf helem cond x = if cond then helem x else x
