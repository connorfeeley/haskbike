-- |

module Database.BikeShare.ImportExport
     ( exportDbTestData
     , importDbTestData
     ) where

import qualified API.StationInformation                as AT
import qualified API.StationStatus                     as AT

import           AppEnv

import           Control.Lens
import           Control.Monad

import           Data.Aeson                            ( eitherDecode, encode )
import qualified Data.ByteString.Lazy                  as L
import           Data.Time

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation
import           Database.BikeShare.StationStatus
import           Database.BikeShare.Utils


-- * Export functions.

{- |
Export table data to a JSON file.

>>> exportDbTestData "test/dumps/" (fromGregorian 2023 10 30) (fromGregorian 2023 10 30)
-}
exportDbTestData :: FilePath -> Day -> Day -> IO FilePath
exportDbTestData outputDir startDay endDay = do
  exportDbTestDataInfo   outputDir ("station_information_" <> show endDay <> ".json")

  exportDbTestDataStatus outputDir ("station_status_" <> show startDay <> "_" <> show endDay <> ".json")
    (UTCTime startDay (timeOfDayToTime midnight)) (UTCTime endDay (timeOfDayToTime (TimeOfDay 12 0 0)))

  pure outputDir


{- |
Export station information to a JSON file.

>>> exportDbTestDataInfo "test/dumps/" "station_information_2023-10-30.json"
-}
exportDbTestDataInfo :: FilePath -> FilePath -> IO FilePath
exportDbTestDataInfo outputDir filePrefix = do
  info <- runWithAppM "haskbike" $ do
    withPostgres $ runSelectReturningList $ select $ do
      all_ (bikeshareDb ^. bikeshareStationInformation)

  -- Convert to API type.
  let info' = map fromBeamStationInformationToJSON info

  let infoJson :: L.ByteString = encode info'

  L.writeFile outputFile infoJson

  pure outputFile
  where
    outputFile = outputDir <> filePrefix


{- |
Export station status to a JSON file.

>>> exportDbTestDataStatus "test/dumps/" "station_status_2023-10-29_2023-10-30.json" (UTCTime (fromGregorian 2023 10 29) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
-}
exportDbTestDataStatus :: FilePath -> FilePath -> UTCTime -> UTCTime -> IO FilePath
exportDbTestDataStatus outputDir filePrefix startTime endTime = do
  status <- runWithAppM "haskbike" $ do
    withPostgres $ runSelectReturningList $ select $ do
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
    outputFile = outputDir <> filePrefix


-- * Import functions.

{- |
Export table data to a JSON file.

>>> importDbTestData "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestData :: FilePath -> FilePath -> FilePath -> IO ()
importDbTestData inputDir infoFile statusFile = do
  importDbTestDataInfo inputDir infoFile

  importDbTestDataStatus inputDir statusFile


{- |
Import station information from a JSON file.

>>> importDbTestDataInfo "test/dumps/" "station_information_2023-10-30.json"
-}
importDbTestDataInfo :: FilePath -> FilePath -> IO ()
importDbTestDataInfo inputDir filePrefix = do
  infoJson <- L.readFile (inputDir <> filePrefix)
  let info = eitherDecode infoJson :: Either String [AT.StationInformation]

  case info of
    Left err -> do
      putStrLn ("Error decoding JSON dump: " <> err)
      pure ()
    Right info' -> void $ runWithAppM dbnameTest $ insertStationInformation info'

{- |
Import station status from a JSON file.

>>> importDbTestDataStatus "test/dumps/" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestDataStatus :: FilePath -> FilePath -> IO ()
importDbTestDataStatus inputDir filePrefix = do
  statusJson <- L.readFile (inputDir <> filePrefix)
  let status = eitherDecode statusJson :: Either String [AT.StationStatus]

  case status of
    Left err -> do
      putStrLn ("Error decoding JSON dump: " <> err)
      pure ()
    Right status' -> void $ runWithAppM dbnameTest $ insertStationStatus status'
