-- |

module Database.BikeShare.Schema.V002.Migrations where

import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.BikeShare.Schema.V001.BikeShare as V001
import           Database.BikeShare.Schema.V002.BikeShare as V002

migration :: CheckedDatabaseSettings Postgres V001.BikeshareDb
          -> Migration Postgres (CheckedDatabaseSettings Postgres V002.BikeshareDb)
migration oldDb =
  V002.BikeshareDb
    <$> preserve (V001._bikeshareEndpointQueriedType oldDb)
    <*> preserve (V001._bikeshareStationInformation oldDb)
    <*> preserve (V001._bikeshareStationStatus oldDb)
    <*> preserve (V001._bikeshareSystemInformation oldDb)
    <*> preserve (V001._bikeshareSystemInformationCount oldDb)
    <*> preserve (V001._bikeshareQueryLog oldDb)
    -- <*> createTable "film_actor"
    --       (FilmActorT (V0001.FilmId (field "film_id" smallint notNull))
    --                                 (V0001.ActorId (field "actor_id" smallint notNull))
    --                                 V0001.lastUpdateField)
