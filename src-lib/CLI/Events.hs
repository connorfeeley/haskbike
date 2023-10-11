{-# LANGUAGE ParallelListComp #-}
-- |

module CLI.Events
     ( bikeCountsAtMoment
     , dayTimes
     , formatBikeCounts
     ) where

import qualified API.Types                     as AT

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog                         ( log, pattern D )

import           Control.Lens                  hiding ( para )
import           Control.Monad                 ( when )

import           Data.Int                      ( Int32 )
import           Data.List                     ( sortOn )
import           Data.Maybe                    ( fromMaybe )
import           Data.Ord                      ( Down (Down) )
import           Data.Proxy
import           Data.Text.Lazy                ( Text, pack, toStrict, unpack )
import           Data.Time                     ( addDays )

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.BikeShare
import           Database.BikeShare.Operations

import           Fmt

import           Prelude                       hiding ( log )

import           ReportTime                    ( Day, TimeOfDay (..), fromGregorian, reportTime )

import           System.Console.ANSI

import qualified Text.PrettyPrint.Boxes        as Box


bikeCountsAtMoment :: Day -> TimeOfDay -> App (Day, TimeOfDay, Int32, Int32, Int32, Int32)
bikeCountsAtMoment day timeOfDay = do
  statusForMoment <- queryAllStationsStatusBeforeTime <$> withConn <*> pure (reportTime day timeOfDay) >>= liftIO
  pure ( day
       , timeOfDay
       , totalBoost statusForMoment
       , totalIconic statusForMoment
       , totalEbikeEfit statusForMoment
       , totalEbikeEfitG5 statusForMoment
       )

-- | Create a list of (Day, TimeOfDay).
dayTimes :: [(Day, TimeOfDay)]
dayTimes = [(addDays n refDay, TimeOfDay h 0 0) | n <- [0..17], h <- [0,2..22]]
  where refDay = fromGregorian 2023 09 24  -- Replace with reference day.


totalBoost, totalIconic, totalEbikeEfit, totalEbikeEfitG5 :: Num (Columnar f Int32) => [StationStatusT f] -> Columnar f Int32
totalBoost bikeCount            = sum $ map (^. vehicle_types_available_boost)   bikeCount
totalIconic bikeCount           = sum $ map (^. vehicle_types_available_iconic)  bikeCount
totalEbikeEfit bikeCount        = sum $ map (^. vehicle_types_available_efit)    bikeCount
totalEbikeEfitG5 bikeCount      = sum $ map (^. vehicle_types_available_efit_g5) bikeCount

formatBikeCounts :: [(Day, TimeOfDay, Int32, Int32, Int32, Int32)] -> IO ()
formatBikeCounts allCounts = Box.printBox table
  where
    col_day  = Box.vcat Box.left (showFn Dull White "Date"    : map (showFn Dull Green   . show) (toListOf (traverse . _1) allCounts))
    col_time = Box.vcat Box.left (showFn Dull White "Time"    : map (showFn Vivid White  . show) (toListOf (traverse . _2) allCounts))

    col1 = Box.vcat Box.left (showFn Dull White  "Total"      : [(showFn Vivid Red    . show) (c + d + e +f) | (a, b, c, d, e, f) <- allCounts, let s = e])
    col2 = Box.vcat Box.left (showFn Dull Green  "Mechanical" : [(showFn Vivid Green  . show) (c + d)        | (a, b, c, d, e, f) <- allCounts, let s = e])
    col3 = Box.vcat Box.left (showFn Dull Red    "E-Bikes"    : [(showFn Vivid Red    . show) (f + s)        | (a, b, c, d, e, f) <- allCounts, let s = e])
    col4 = Box.vcat Box.left (showFn Dull Yellow "E-Fit"      : [(showFn Dull Yellow  . show) e              | (a, b, c, d, e, f) <- allCounts, let s = e])
    col5 = Box.vcat Box.left (showFn Dull Yellow "E-Fit G5"   : [(showFn Vivid Yellow . show) f              | (a, b, c, d, e, f) <- allCounts, let s = e])

    showFn :: ColorIntensity -> Color -> String -> Box.Box
    showFn intensity colour = Box.text . (unpack . colouredText intensity colour . pack)
    table = Box.hsep 2 Box.left [col_day, col_time, col1, col2, col3, col4, col5]
