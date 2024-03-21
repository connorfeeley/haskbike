-- | Test the client functions.
module TestPoll where

import           Colog                       ( Severity (Warning), logInfo )

import           Control.Exception           ( SomeException, try )
import           Control.Monad               ( void )

import qualified Data.Text.Lazy              as TL
import           Data.Time                   ( getCurrentTimeZone )

import           Haskbike.API.Client
import           Haskbike.AppEnv
import           Haskbike.CLI.Options        ( PollOptions (..), PopulateStatusChangesOpt (..) )
import qualified Haskbike.CLI.Poll           as Poll
import           Haskbike.Database.BikeShare ( migrateDB )
import           Haskbike.Database.Utils

import           Network.HTTP.Client         ( newManager )
import           Network.HTTP.Client.TLS     ( tlsManagerSettings )

import           Prelude                     hiding ( log, unwords )

import           Test.Tasty.HUnit

import           Text.Pretty.Simple.Extras

import           UnliftIO                    ( liftIO, timeout )


unit_poll :: IO ()
unit_poll = do
  timeZone <- getCurrentTimeZone

  -- Establish a connection to the database, drop all tables, and re-initialize it.
  -- Establish a connection to the database.
  connInfo <- mkDbConnectInfo dbnameTest
  connPool <- mkDatabaseConnectionPool connInfo
  runWithAppM dbnameTest dropTables

  clientManager <- liftIO $ newManager tlsManagerSettings

  -- Create the application environment.
  let env = mainEnv Warning False False timeZone connPool clientManager

  -- Log the database connection parameters.
  runAppM env $
    logInfo $ "Connected to database using: " <> (TL.toStrict . pShowCompact) connInfo
  runAppM env migrateDB
  runAppM env doPoll
  where
    pollOpts = PollOptions NeverPopulate

    doPoll :: AppM ()
    doPoll = void $ timeout 1000000 $ do -- Terminate after 1 second
      (void . Poll.pollClient) pollOpts
