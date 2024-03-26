-- |

module Haskbike.Database.ImportExport
     ( exportDbTestData
     , importDbTestData
     ) where

import qualified Codec.Compression.Zstd                      as Z

import           Colog                                       ( logError, logInfo )

import           Control.Lens
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Aeson                                  ( eitherDecode, encode )
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.Text                                   as T
import           Data.Time

import           Database.Beam

import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.StationStatus                  as AT
import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Operations
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus

import           UnliftIO


-- * Export functions.

{- |
Export table data to a JSON file.

>>> exportDbTestData "test/dumps/" (fromGregorian 2023 10 30) (fromGregorian 2023 10 30)
-}
exportDbTestData :: (MonadCatch m, HasEnv env m) => FilePath -> Day -> Day -> m (FilePath, FilePath)
exportDbTestData outputDir startDay endDay = do
  logInfo "Querying station status in range and related station information."
  result <- queryDbTestData (dayMidnight startDay) (dayNoon endDay)

  -- Convert to API type.
  let info'   = map (fromBeamStationInformationToJSON . fst) result
  let status' = map (fromBeamStationStatusToJSON      . snd) result

  infoPath   <- liftIO $ writeDbTestData outputDir infoPrefix (encode info')
  statusPath <- liftIO $ writeDbTestData outputDir statusPrefix (encode status')
  pure (infoPath, statusPath)
  where
    dayMidnight day = UTCTime day (timeOfDayToTime midnight)
    dayNoon     day = UTCTime day (timeOfDayToTime (TimeOfDay 12 0 0))
    statusPrefix = "station_status_"      <> show startDay <> "_" <> show endDay <> ".json"
    infoPrefix   = "station_information_" <> show startDay <> "_" <> show endDay <> ".json"

-- (UTCTime (fromGregorian 2023 10 29) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
queryDbTestData :: (HasEnv env m, MonadCatch m) => UTCTime -> UTCTime -> m [(StationInformation, StationStatus)]
queryDbTestData startTime endTime = do
  logInfo "Exporting station information."
  withPostgres $ runSelectReturningList $ select $ do
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_' (sqlBool_ ((status ^. statusLastReported) >=. val_ startTime) &&?.
             sqlBool_ ((status ^. statusLastReported) <=. val_ endTime))
    info   <- related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status)
    -- guard_' (sqlBool_ ((info ^. infoReported) <=. val_ endTime) &&?.
    --          (_statusInfoId . _statusCommon) status `references_'` info)
    -- guard_' ((info ^. infoStationId) ==?. (status ^. statusInfoId . unInformationStationId))
    pure (info, status)


writeDbTestData :: FilePath -> FilePath -> BL.ByteString -> IO FilePath
writeDbTestData outputDir filePrefix jsonEncoded = do
  BL.writeFile outputFile compressedJson
  pure outputFile
  where
    outputFile = outputDir <> filePrefix <> ".zst"
    compressedJson = BL.fromStrict (Z.compress 6 (BL.toStrict jsonEncoded))


-- * Import functions.

{- |
Export table data to a JSON file.

>>> importDbTestData "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestData :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
                 => FilePath -> FilePath -> FilePath -> m ([StationInformationT Identity], [StationStatusT Identity])
importDbTestData inputDir infoFile statusFile = do
  info   <- importDbTestDataInfo inputDir infoFile
  status <- importDbTestDataStatus inputDir statusFile

  pure (info, status)


{- |
Import station information from a JSON file.

>>> importDbTestDataInfo "test/dumps/" "station_information_2023-10-30.json"
-}
importDbTestDataInfo :: (MonadCatch m, HasEnv env m)
                     => FilePath -> FilePath -> m [StationInformationT Identity]
importDbTestDataInfo inputDir filePrefix = do
  contents <- liftIO . B.readFile $ inputDir <> filePrefix
  case Z.decompress contents of
    Z.Skip                    -> error "Either frame was empty, or compression was done in streaming mode"
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let status :: Either String [AT.StationInformation] = eitherDecode . BL.fromStrict $ decompressed

      reported <- liftIO getCurrentTime
      case status of
        Left err    -> error err
        Right info' -> insertStationInformation reported info'

{- |
Import station status from a JSON file.

>>> importDbTestDataStatus "test/dumps/" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestDataStatus :: (MonadCatch m, HasEnv env m)
                       => FilePath -> FilePath -> m [StationStatus]
importDbTestDataStatus inputDir filePrefix = do
  contents <- liftIO . B.readFile $ inputDir <> filePrefix <> ".zst"
  case Z.decompress contents of
    Z.Skip                    -> error "Either frame was empty, or compression was done in streaming mode"
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let status :: Either String [AT.StationStatus] = eitherDecode . BL.fromStrict $ decompressed

      case status of
        Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
        Right status' -> insertStationStatus status'
