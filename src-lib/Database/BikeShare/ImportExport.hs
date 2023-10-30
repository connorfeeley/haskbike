-- |

module Database.BikeShare.ImportExport where
import           AppEnv

import           Control.Lens

import           Data.Aeson           ( encode )
import qualified Data.ByteString.Lazy as L
import           Data.Time

import           Database.Beam
import           Database.BikeShare

import           System.FilePath


-- * Export functions.

{- |
Export table data to a JSON file.

>>> exportDbTestData "test/dumps" (fromGregorian 2023 10 29) (fromGregorian 2023 10 30)
-}
exportDbTestData :: FilePath -> Day -> Day -> IO FilePath
exportDbTestData outputDir startDay endDay = do
  putStrLn "Exporting station information"
  infoFile   <- exportDbTestDataInfo   outputDir ("station_information_" <> show endDay <> ".json")
  putStrLn ("Exported station information JSON to " <> relOutputFile infoFile)

  putStrLn "Exporting station status"
  statusFile <- exportDbTestDataStatus outputDir ("station_status_" <> show startDay <> "_" <> show endDay <> ".json")
                (UTCTime startDay (timeOfDayToTime midnight)) (UTCTime endDay (timeOfDayToTime midnight))
  putStrLn ("Exported station status JSON to "      <> relOutputFile statusFile)

  pure outputDir
  where
    relOutputFile = makeRelative outputDir


{- |
Export station information to a JSON file.

>>> exportDbTestDataInfo "test/dumps" "station_information_2023-10-30.json"
-}
exportDbTestDataInfo :: FilePath -> FilePath -> IO FilePath
exportDbTestDataInfo outputDir filePrefix = do
  info <- runWithAppM "haskbike" $ do
    withPostgres $ runSelectReturningList $ select $ do
      -- insertStationInformation $ info   ^. response_data . unInfoStations
      all_ (bikeshareDb ^. bikeshareStationInformation)

  -- Convert to API type.
  let info' = map fromBeamStationInformationToJSON info

  let infoJson :: L.ByteString = encode info'

  L.writeFile outputFile infoJson

  pure outputFile
  where
    outputFile = makeRelative outputDir filePrefix


{- |
Export station status to a JSON file.

>>> exportDbTestDataStatus "test/dumps" "station_status_2023-10-29_2023-10-30.json" (UTCTime (fromGregorian 2023 10 29) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
-}
exportDbTestDataStatus :: FilePath -> FilePath -> UTCTime -> UTCTime -> IO FilePath
exportDbTestDataStatus outputDir filePrefix startTime endTime = do
  status <- runWithAppM "haskbike" $ do
    withPostgres $ runSelectReturningList $ select $ do
      -- insertStationInformation $ info   ^. response_data . unInfoStations
      status <- all_ (bikeshareDb ^. bikeshareStationStatus)
      guard_ ((status ^. statusLastReported) >=. val_ startTime &&.
              (status ^. statusLastReported) <=. val_ endTime)
      pure status

  -- Convert to API type.
  let status' = map fromBeamStationStatusToJSON status

  let statusJson :: L.ByteString = encode status'

  L.writeFile outputFile statusJson

  pure outputFile
  where
    outputFile = makeRelative outputDir filePrefix
