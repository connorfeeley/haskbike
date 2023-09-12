-- | Utility functions for database operations.

module Database.Utils where

import qualified Database.BikeShare         as DBS

import           Control.Lens
import           Data.String                (fromString)
import           Text.Pretty.Simple

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Migrations        (migrateDB)
import           Database.PostgreSQL.Simple
import           Database.Types

-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Establish a connection to the database.
connectDb :: IO Connection
connectDb =
  connectPostgreSQL $ fromString "host=localhost port=5432 dbname=haskbike connect_timeout=10"

setupDatabase :: IO Connection
setupDatabase = do
  -- Connect to the database.
  conn <- connectDb

  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  -- Initialize the database.
  _ <- migrateDB conn

  pPrintString "Database reinitialization complete."

  pure conn


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty pPrintCompactOpt
  where
    pPrintCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks conn =
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
  info <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
  status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
  guard_ (_status_station_id status `references_` info &&. status^.status_num_docks_disabled >. 0)
  pure ( info^.info_name
       , status^.status_num_docks_disabled
       )

-- | Helper function to print disabled docks.
printDisabledDocks :: IO ()
printDisabledDocks = (connectDb >>= queryDisabledDocks) >>= pPrintCompact
