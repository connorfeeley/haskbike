-- |

module Haskbike.Database.ImportExport
     ( exportDbTestData
     , importDbTestData
     , importDbTestDataInfo
     , importDbTestDataInfo'
     , importDbTestDataNew
     , importDbTestDataStatus
     , importDbTestDataStatus'
     , writeDbTestData
     , writeDbTestData'
     ) where

import qualified Codec.Compression.Zstd                      as Z
import qualified Codec.Compression.Zstd.Lazy                 as ZL

import           Colog                                       ( logDebug, logError, logInfo )

import           Control.Lens
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Aeson                                  ( ToJSON, eitherDecode, eitherDecodeStrict, encode )
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import           Data.List                                   ( nub )
import qualified Data.Text                                   as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                      ( Postgres )
import           Database.Beam.Postgres.Full                 ( lateral_ )

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
exportDbTestData :: (MonadCatch m, HasEnv env m) => FilePath -> Maybe Int -> Day -> Day -> m (FilePath, FilePath)
exportDbTestData outputDir stationId startDay endDay = do
  logInfo "Querying station status in range and related station information."
  result <- queryDbTestData stationId (dayMidnight startDay) (dayNoon endDay)

  -- Convert to API type.
  let info'   = nub . map fst $ result
  let status' = nub . map (fromBeamStationStatusToJSON . snd) $ result

  -- Lazy variants:
  -- infoPath   <- liftIO $ writeDbTestData outputDir infoPrefix   info'
  -- statusPath <- liftIO $ writeDbTestData outputDir statusPrefix status'

  -- Strict variants:
  infoPath   <- liftIO $ writeDbTestData' outputDir infoPrefix   info'
  statusPath <- liftIO $ writeDbTestData' outputDir statusPrefix status'
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

>>> queryDbTestData (Just 7001)
  (UTCTime (fromGregorian 2024 01 03) (timeOfDayToTime midnight))
  (UTCTime (fromGregorian 2024 01 04) (timeOfDayToTime (TimeOfDay 12 0 0)))
-}
queryDbTestData :: (HasEnv env m, MonadCatch m) => Maybe Int -> UTCTime -> UTCTime -> m [(StationInformation, StationStatus)]
queryDbTestData stationId startTime endTime = do
  logInfo "Querying data to export."
  withPostgres $ runSelectReturningList $ select $ do
    status <- stationStatusQuery stationId
    guard_' (sqlBool_ ((status ^. statusLastReported) >=. val_ startTime) &&?.
             sqlBool_ ((status ^. statusLastReported) <=. val_ endTime))
    info   <- lateral_ status $ \status' -> do
              nub_ $ related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status')
              -- pgNubBy_ (\inf -> (_infoStationId inf, _infoReported inf)) $
              --   related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status')
                -- filter_ (\inf -> (_statusInfoId . _statusCommon) status' `references_` inf) $
                -- all_ (bikeshareDb ^. bikeshareStationInformation)
    pure (info, status)

-- | Query for station status.
stationStatusQuery :: Integral a => Maybe a -> Q Postgres BikeshareDb s (StationStatusT (QExpr Postgres s))
stationStatusQuery Nothing = all_ (bikeshareDb ^. bikeshareStationStatus)
stationStatusQuery (Just stationId) = filter_' (\ss -> (val_ . fromIntegral) stationId ==?. (_unInformationStationId . _statusInfoId . _statusCommon) ss)
                                      (all_ (bikeshareDb ^. bikeshareStationStatus))

-- | Write data to a JSON file.
writeDbTestData :: ToJSON a => FilePath -> FilePath -> a -> IO FilePath
writeDbTestData outputDir filePrefix toEncode = do
  BL.writeFile outputFile compressedJson
  pure outputFile
  where
    outputFile = outputDir <> filePrefix <> ".zst"
    compressedJson = ZL.compress 6 (encode toEncode)

-- | Strict version of 'writeDbTestData'.
writeDbTestData' :: ToJSON a => FilePath -> FilePath -> a -> IO FilePath
writeDbTestData' outputDir filePrefix toEncode = do
  B.writeFile outputFile compressedJson
  pure outputFile
  where
    outputFile = outputDir <> filePrefix <> ".zst"
    compressedJson = Z.compress 6 ((BL.toStrict . encode) toEncode)


-- * Import functions.

{- |
Export table data to a JSON file.

>>> importDbTestDataNew "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-29_2023-10-30.json"
-}
importDbTestDataNew :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
                    => FilePath -> FilePath -> FilePath -> m ([StationInformationT Identity], [StationStatusT Identity])
importDbTestDataNew inputDir infoFile statusFile = do
  info   <-   importDbTestDataInfo inputDir infoFile
  status <- importDbTestDataStatus inputDir statusFile

  pure (info, status)


{- |
Import station information from a JSON file.

>>> importDbTestDataInfo "test/dumps/" "station_information_2023-10-30.json"
-}
importDbTestDataInfo :: (MonadCatch m, HasEnv env m)
                     => FilePath -> FilePath -> m [StationInformationT Identity]
importDbTestDataInfo inputDir filePrefix = do
  logDebug $ T.pack ("Reading station information from file: " <> show filePath)
  contents <- liftIO . BL.readFile $ filePath
  logDebug "Decompressing station information."
  let decompressed = ZL.decompress contents
  logDebug "Parsing station information."
  let info :: Either String [StationInformation] = eitherDecode decompressed

  case info of
    Left err    -> error err
    Right info' -> logDebug "Inserting station information." >> do
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
  logDebug $ T.pack ("Reading station status from file: " <> show filePath)
  contents <- liftIO . BL.readFile $ filePath
  logDebug "Decompressing station status."
  let decompressed = ZL.decompress contents
  logDebug "Parsing station status."
  let status :: Either String [AT.StationStatus] = eitherDecode decompressed

  case status of
    Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
    Right status' -> logDebug "Inserting station status." >> insertStationStatus status'
  where
    filePath = inputDir <> filePrefix <> ".zst"

-- | Strict version of 'importDbTestDataInfo'.
importDbTestDataInfo' :: (MonadCatch m, HasEnv env m)
                      => FilePath -> FilePath -> m [StationInformationT Identity]
importDbTestDataInfo' inputDir filePrefix = do
  logDebug $ T.pack ("Reading station information from file: " <> show filePath)
  contents <- liftIO . B.readFile $ filePath
  logDebug "Decompressing station information."
  case Z.decompress contents of
    Z.Skip                    -> error $ "StationInformation: either frame was empty, or compression was done in streaming mode for path: " <> filePath
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      logDebug "Parsing station information."
      let info :: Either String [StationInformation] = eitherDecodeStrict decompressed

      case info of
        Left err    -> error err
        Right info' -> logDebug "Inserting station information." >> do
          let infoWithReported = map (\i -> (_infoReported i, fromBeamStationInformationToJSON i)) info'
          insertStationInformation infoWithReported
  where
    filePath = inputDir <> filePrefix <> ".zst"

-- | Strict version of 'importDbTestDataStatus'.
importDbTestDataStatus' :: (MonadCatch m, HasEnv env m)
                        => FilePath -> FilePath -> m [StationStatus]
importDbTestDataStatus' inputDir filePrefix = do
  logDebug $ T.pack ("Reading station status from file: " <> show filePath)
  contents <- liftIO . B.readFile $ filePath
  logDebug "Decompressing station status."
  case Z.decompress contents of
    Z.Skip                    -> error $ "StationStatus: either frame was empty, or compression was done in streaming mode for path " <> filePath
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      logDebug "Parsing station status."
      let status :: Either String [AT.StationStatus] = eitherDecodeStrict decompressed

      case status of
        Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
        Right status' -> logDebug "Inserting station status." >> insertStationStatus status'
  where
    filePath = inputDir <> filePrefix <> ".zst"


-- * Old importers.

importTestDataStatus :: (MonadCatch m, HasEnv env m)
                     => FilePath -> FilePath -> m [StationStatus]
importTestDataStatus inputDir filePrefix = do
  logDebug $ T.pack ("Reading station status from file: " <> show filePath)
  contents <- liftIO . BL.readFile $ filePath
  logDebug "Parsing station status."
  let status :: Either String [AT.StationStatus] = eitherDecode contents

  case status of
    Left err      -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
    Right status' -> logDebug "Inserting station status." >> insertStationStatus status'
  where
    filePath = inputDir <> filePrefix

importTestDataInfo :: (MonadCatch m, HasEnv env m)
                   => FilePath -> FilePath -> m [StationInformation]
importTestDataInfo inputDir filePrefix = do
  logDebug $ T.pack ("Reading station information from file: " <> show filePath)
  contents <- liftIO . BL.readFile $ filePath
  logDebug "Parsing station information."
  let info :: Either String [AT.StationInformation] = eitherDecode contents

  ct <- liftIO getCurrentTime
  case info of
    Left err    -> (logError . T.pack) ("Error decoding JSON dump: " <> err) >> pure []
    Right info' -> logDebug "Inserting station information." >> insertStationInformation (map (ct, ) info')
  where
    filePath = inputDir <> filePrefix


importDbTestData :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
                    => FilePath -> FilePath -> FilePath -> m ([StationInformationT Identity], [StationStatusT Identity])
importDbTestData inputDir infoFile statusFile = do
  info   <-   importTestDataInfo inputDir infoFile
  status <- importTestDataStatus inputDir statusFile

  pure (info, status)
