-- |

module Haskbike.Server.Components.PerformanceData where


import qualified Data.Text                                 as T
import           Data.Time

import           GHC.Generics                              ( Generic )

import qualified Haskbike.Database.Operations.Factors      as DB
import qualified Haskbike.Database.Tables.StationOccupancy as DB
import           Haskbike.Server.Data.EmptyFullData
import           Haskbike.Server.Page.Utils

import           Lucid


-- | Combination of status integrals and factors.
data PerformanceData where
  PerformanceData :: { performanceIntegrals :: DB.StatusIntegral
                     , performanceFactors   :: DB.StatusFactor
                     , performanceEmptyFull :: DB.EmptyFull
                     } -> PerformanceData
  deriving (Generic, Show, Eq)

integralToPerformanceData :: DB.EmptyFull -> DB.StatusIntegral -> PerformanceData
integralToPerformanceData emptyFull integral = PerformanceData integral (DB.integralToFactor integral) emptyFull

instance ToHtml PerformanceData where
  toHtmlRaw = toHtml
  toHtml params =
    div_ $ do
      div_ [class_ "tooltip"] $ do
        label_ [for_ "performance-data"] (h3_ "Performance")
        div_ [class_ "tooltip-bottom", style_ "min-width: 250px;"] $ do -- Tooltip content
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesAvailable < 0.1)  (factorOf bikesAvailable (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesDisabled  > 0.1)  (factorOf bikesDisabled  (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksAvailable < 0.1)  (factorOf docksAvailable (Just 5)))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksDisabled  > 0.05) (factorOf docksDisabled  (Just 5)))
      div_ [id_ "performance-data"] $ do
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesAvailable < 0.1)  (factorOf bikesAvailable (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bike disabled:  " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (bikesDisabled  > 0.1)  (factorOf bikesDisabled  (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock available: " <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksAvailable < 0.1)  (factorOf docksAvailable (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Dock disabled: "  <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (docksDisabled  > 0.05) (factorOf docksDisabled  (Just 2)))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Time empty: "     <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (timeGreaterThan emptyTime 100) ((toHtml . formatDiffTime) emptyTime))
        p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Time full: "      <> span_ [class_ "pure-u-1-2"] (elemIf strong_ (timeGreaterThan fullTime  100) ((toHtml . formatDiffTime) fullTime))

    where
      factorOf :: Monad m => Double -> Maybe Int -> HtmlT m ()
      factorOf factor truncateNum =
        case truncateNum of
          Just decimals -> (toHtml . toPercentage decimals) factor <> "%"
          Nothing       ->  showth factor <> "%"

      timeGreaterThan :: Maybe NominalDiffTime -> Int -> Bool
      timeGreaterThan (Just t1) t2 = t1 > minutes t2
      timeGreaterThan Nothing _t2  = False

      getFactor factor = (factor . performanceFactors) params

      bikesAvailable = getFactor DB.statusFactorBikesAvailable
      bikesDisabled  = getFactor DB.statusFactorBikesDisabled
      docksAvailable = getFactor DB.statusFactorDocksAvailable
      docksDisabled  = getFactor DB.statusFactorDocksDisabled
      emptyTime      = (DB._emptyTime . performanceEmptyFull) params
      fullTime       = (DB._fullTime  . performanceEmptyFull) params
      minutes :: Int -> NominalDiffTime
      minutes m      = fromIntegral m * 60

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t) :: Int) / t
  where
    t = 10^n

toPercentage :: Int -> Double -> T.Text
toPercentage decimals factor =
  if percent < 0.05
  then T.pack ("~" ++ show (0 :: Integer))
  else (T.pack . show) percent
  where percent = truncate' (factor * 100) decimals

elemIf :: (t -> t) -> Bool -> t -> t
elemIf helem cond x = if cond then helem x else x
