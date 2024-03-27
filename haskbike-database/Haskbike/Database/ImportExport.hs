-- |

module Haskbike.Database.ImportExport
     ( exportDbTestData
     , importDbTestData
     , importDbTestDataInfo
     , importDbTestDataInfo'
     , importDbTestDataStatus
     , importDbTestDataStatus'
     ) where

import qualified Codec.Compression.Zstd                      as Z
import qualified Codec.Compression.Zstd.Lazy                 as ZL

import           Colog                                       ( logError, logInfo )

import           Control.Lens
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Aeson                                  ( eitherDecode, encode )
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.Text                                   as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                      ( Postgres )

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
exportDbTestData :: (MonadCatch m, HasEnv env m) => FilePath -> Maybe Int -> Day -> Day -> m (FilePath, FilePath)
exportDbTestData outputDir stationId startDay endDay = do
  logInfo "Querying station status in range and related station information."
  result <- queryDbTestData stationId (dayMidnight startDay) (dayNoon endDay)

  -- Convert to API type.
  let info'   = map fst result
  let status' = map (fromBeamStationStatusToJSON      . snd) result

  infoPath   <- liftIO $ writeDbTestData outputDir infoPrefix (encode info')
  statusPath <- liftIO $ writeDbTestData outputDir statusPrefix (encode status')
  pure (infoPath, statusPath)
  where
    dayMidnight day = UTCTime day (timeOfDayToTime midnight)
    dayNoon     day = UTCTime day (timeOfDayToTime (TimeOfDay 12 0 0))
    statusPrefix = "station_status_"      <> stationIdPart stationId <> "_" <> show startDay <> "_" <> show endDay <> ".json"
    infoPrefix   = "station_information_" <> stationIdPart stationId <> "_" <> show startDay <> "_" <> show endDay <> ".json"
    stationIdPart Nothing    = "all"
    stationIdPart (Just sId) = show sId

--
{- |
Query for database export.

>>> queryDbTestData (UTCTime (fromGregorian 2024 01 03) (timeOfDayToTime midnight))
  (UTCTime (fromGregorian 2024 01 04) (timeOfDayToTime (TimeOfDay 12 0 0)))
-}
queryDbTestData :: (HasEnv env m, MonadCatch m) => Maybe Int -> UTCTime -> UTCTime -> m [(StationInformation, StationStatus)]
queryDbTestData stationId startTime endTime = do
  logInfo "Exporting station information."
  withPostgres $ runSelectReturningList $ select $ do
    status <- stationStatusQuery stationId
    guard_' (sqlBool_ ((status ^. statusLastReported) >=. val_ startTime) &&?.
             sqlBool_ ((status ^. statusLastReported) <=. val_ endTime))
    info   <- related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status)
    pure (info, status)

stationStatusQuery :: Integral a => Maybe a -> Q Postgres BikeshareDb s (StationStatusT (QExpr Postgres s))
stationStatusQuery Nothing = all_ (bikeshareDb ^. bikeshareStationStatus)
stationStatusQuery (Just stationId) =
  filter_' (\ss -> (val_ . fromIntegral) stationId ==?. (_unInformationStationId . _statusInfoId . _statusCommon) ss)
  (all_ (bikeshareDb ^. bikeshareStationStatus))

writeDbTestData :: FilePath -> FilePath -> BL.ByteString -> IO FilePath
writeDbTestData outputDir filePrefix jsonEncoded = do
  BL.writeFile outputFile compressedJson
  pure outputFile
  where
    outputFile = outputDir <> filePrefix <> ".zst"
    compressedJson = ZL.compress 6 jsonEncoded


-- * Import functions.

{- |
Export table data to a JSON file.

>>> importDbTestData "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestData :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
                 => FilePath -> FilePath -> FilePath -> m ([StationInformationT Identity], [StationStatusT Identity])
importDbTestData inputDir infoFile statusFile = do
  info   <-   importDbTestDataInfo' inputDir infoFile
  status <- importDbTestDataStatus' inputDir statusFile

  pure (info, status)


{- |
Import station information from a JSON file.

>>> importDbTestDataInfo "test/dumps/" "station_information_2023-10-30.json"
-}
importDbTestDataInfo :: (MonadCatch m, HasEnv env m)
                     => FilePath -> FilePath -> m [StationInformationT Identity]
importDbTestDataInfo inputDir filePrefix = do
  contents <- liftIO . BL.readFile $ filePath
  let decompressed = ZL.decompress contents
  let info :: Either String [StationInformation] = eitherDecode decompressed

  case info of
    Left err    -> error err
    Right info' -> do
      let infoWithReported = map (\i -> (_infoReported i, fromBeamStationInformationToJSON i)) info'
      insertStationInformation infoWithReported
  where
    filePath = inputDir <> filePrefix <> ".zst"

{- |
Import station status from a JSON file.

>>> importDbTestDataStatus' "test/dumps/" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestDataStatus :: (MonadCatch m, HasEnv env m)
                       => FilePath -> FilePath -> m [StationStatus]
importDbTestDataStatus inputDir filePrefix = do
  contents <- liftIO . BL.readFile $ filePath
  let decompressed = ZL.decompress contents
  let status :: Either String [AT.StationStatus] = eitherDecode decompressed

  case status of
    Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
    Right status' -> insertStationStatus status'
  where
    filePath = inputDir <> filePrefix <> ".zst"

importDbTestDataInfo' :: (MonadCatch m, HasEnv env m)
                      => FilePath -> FilePath -> m [StationInformationT Identity]
importDbTestDataInfo' inputDir filePrefix = do
  contents <- liftIO . B.readFile $ filePath
  case Z.decompress contents of
    Z.Skip                    -> error $ "StationInformation: either frame was empty, or compression was done in streaming mode for path: " <> filePath
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let info :: Either String [StationInformation] = eitherDecode . BL.fromStrict $ decompressed

      case info of
        Left err    -> error err
        Right info' -> do
          let infoWithReported = map (\i -> (_infoReported i, fromBeamStationInformationToJSON i)) info'
          insertStationInformation infoWithReported
  where
    filePath = inputDir <> filePrefix <> ".zst"

{- |
Import station status from a JSON file.

>>> importDbTestDataStatus' "test/dumps/" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestDataStatus' :: (MonadCatch m, HasEnv env m)
                        => FilePath -> FilePath -> m [StationStatus]
importDbTestDataStatus' inputDir filePrefix = do
  contents <- liftIO . B.readFile $ filePath
  case Z.decompress contents of
    Z.Skip                    -> error $ "StationStatus: either frame was empty, or compression was done in streaming mode for path " <> filePath
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let status :: Either String [AT.StationStatus] = eitherDecode . BL.fromStrict $ decompressed

      case status of
        Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
        Right status' -> insertStationStatus status'
  where
    filePath = inputDir <> filePrefix <> ".zst"
