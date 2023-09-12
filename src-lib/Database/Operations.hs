-- | This module contains the operations that can be performed on the database.

module Database.Operations where

import           API.Types                                (StationInformationResponse (..),
                                                           StationStatusResponse (..))
import qualified Database.BikeShare                       as DBS
import           Database.Types
import           Database.Utils

import           Control.Lens
import           Text.Pretty.Simple

import           Client
import           Control.Exception                        (Exception (displayException))
import           Control.Monad                            (void)
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Network.HTTP.Client                      (newManager)
import           Network.HTTP.Client.TLS                  (tlsManagerSettings)

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks conn =
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
  info   <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
  status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
  guard_ (_status_station_id status `references_` info &&. status^.status_num_docks_disabled >. 0)
  pure ( info^.info_name
       , status^.status_num_docks_disabled
       )

-- | Helper function to print disabled docks.
printDisabledDocks :: IO ()
printDisabledDocks = (connectDb >>= queryDisabledDocks) >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Connection -> IO [(StationInformation, StationStatus)]
queryStationStatus conn = do
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
    info   <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
    status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
    guard_ (_status_station_id status `references_` info)
    -- station_status <- leftJoin_ (all_(DBS.bikeshareDb ^. DBS.bikeshareStationStatus))
    --   (\station_status -> _station_id station_status `references_` station_information)
    -- guard_ (isJust_ station_status)
    pure (info, status)

-- | Query database for station status, returning the number of bikes and docks available and disabled.
queryStationStatusFields conn =
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
  info   <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
  status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
  guard_ (_status_station_id status `references_` info)
  pure ( info^.info_name
       , status^.status_num_bikes_available
       , status^.status_num_bikes_disabled
       , status^.status_num_docks_available
       , status^.status_num_docks_disabled
       )

-- | Insert station information into the database.
insertStationInformation :: Connection -> StationInformationResponse -> IO [StationInformation]
insertStationInformation conn stations = do
  runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation (info_stations stations)

-- | Insert station status into the database.
insertStationStatus :: Connection -> StationStatusResponse -> IO [StationStatus]
insertStationStatus conn status = do
  runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus (status_stations status)

insertStationInformationApi :: Connection -> IO ()
insertStationInformationApi conn = do
  clientManager <- newManager tlsManagerSettings
  -- Query API for station information.
  stationInformationResponse <- runQuery clientManager stationInformation

  case stationInformationResponse of
    Left err -> putStrLn $ "Error: " ++ displayException err
    Right info -> do
      -- Insert station information into database.
      void $ -- Suppress return value.
        runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
        insertExpressions $ map fromJSONToBeamStationInformation (info_stations info)
