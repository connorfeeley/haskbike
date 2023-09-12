-- | This module contains the operations that can be performed on the database.

module Database.Operations where

import           API.Types                                (StationInformationResponse (..),
                                                           StationStatusResponse (..))
import           Database.BikeShare
import           Database.Utils

import           Control.Lens

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks conn =
  runBeamPostgresDebug pPrintCompact conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
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
  runBeamPostgresDebug pPrintCompact conn $ runSelectReturningList $ select $ do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_ (_status_station_id status `references_` info)
    -- station_status <- leftJoin_ (all_(bikeshareDb ^. bikeshareStationStatus))
    --   (\station_status -> _station_id station_status `references_` station_information)
    -- guard_ (isJust_ station_status)
    pure (info, status)

-- | Query database for station status, returning the number of bikes and docks available and disabled.
queryStationStatusFields conn =
  runBeamPostgresDebug pPrintCompact conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
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
  runBeamPostgresDebug pPrintCompact conn $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation (info_stations stations)

-- | Insert station status into the database.
insertStationStatus :: Connection -> StationStatusResponse -> IO [StationStatus]
insertStationStatus conn status = do
  runBeamPostgresDebug pPrintCompact conn $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus (status_stations status)
