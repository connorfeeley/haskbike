-- | This module contains expressions for querying the database.

module Database.BikeShare.Expressions
     ( statusBetweenExpr
     ) where

import           Control.Lens           hiding ( reuse, (<.) )

import           Data.Int               ( Int32 )

import           Database.Beam
import           Database.Beam.Backend  ( BeamSqlBackend )
import           Database.Beam.Postgres
import           Database.BikeShare


-- | Expression to query the statuses for a station between two times.
statusBetweenExpr :: (BeamSqlBackend be, HasSqlEqualityCheck be Int32, HasSqlEqualityCheck be ReportTime, be ~ Postgres)
                  => Connection -> Int32 -> ReportTime -> ReportTime -> Q be BikeshareDb s (StationStatusT (QGenExpr QValueContext be s))
statusBetweenExpr conn station_id start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- orderBy_ (asc_ . _d_status_last_reported)
              (all_ (bikeshareDb ^. bikeshareStationStatus))
    guard_ (_d_status_info_id status `references_` info &&.
            _info_station_id info ==. val_ (fromIntegral station_id) &&.
            (status ^. d_status_last_reported) >=. val_ (Just start_time) &&.
            (status ^. d_status_last_reported) <=. val_ (Just end_time))
    pure status
