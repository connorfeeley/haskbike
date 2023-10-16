module Main
     ( appMain
     , main
     ) where


import           AppEnv

import           CLI.Database
import           CLI.Debug
import           CLI.Events
import           CLI.Options
import           CLI.Poll
import           CLI.Query

import           Colog                    ( LogAction, Severity (..), WithLog, cmap, fmtMessage, log, logTextStdout,
                                            pattern D, pattern E, pattern I, usingLoggerT )

import           Control.Monad            ( void, when )
import           Control.Monad.IO.Class   ( MonadIO )

import           Data.Text                ( pack, unwords )
import qualified Data.Text                as Text
import           Data.Text.Lazy           ( toStrict )
import           Data.Time                ( getCurrentTimeZone )

import           Database.BikeShare.Utils

import           Formatting

import           Network.HTTP.Client      ( newManager )
import           Network.HTTP.Client.TLS  ( tlsManagerSettings )

import           Options.Applicative

import           Prelude                  hiding ( log, unwords )

import           UnliftIO                 ( MonadUnliftIO )


main :: IO ()
main = do
  -- Parse command line options.
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts

  -- Log options when verbosity is high enough.
  when (length (optVerbose options) >= 3) $ usingLoggerT logStdoutAction $
    log D $ "Connecting to database using: " <> toStrict (pShowCompact options)

  -- Get the current time zone.
  timeZone <- getCurrentTimeZone

  -- Establish a connection to the database.
  (name, host, port, username, password) <- mkDbParams (optDatabase options)
  let logParams = unwords [ "dbname=" <> pack name
                          , pack host
                          , pack port
                          , pack username
                          , pack "password=" <> pack (map (const '*') password) -- Obfuscate password in logs.
                          ]
  usingLoggerT logStdoutAction $
    log I $ "Connecting to database using: " <> logParams
  conn <- connectDbName name host port username password

  clientManager <- liftIO $ newManager tlsManagerSettings

  -- Create the application environment.
  let env = mainEnv (logLevel options) (logDatabase options) (optLogRichOutput options) timeZone conn clientManager

  -- Log the database connection parameters.
  runApp env (log I $ "Connected to database using: " <> logParams)

  -- Run the application.
  runApp env (appMain options)
  where
    -- | Log database operations only when '--log-database' is given.
    logDatabase = optLogDatabase

    -- | Logging action for stdout.
    logStdoutAction :: LogAction IO Message
    logStdoutAction = cmap fmtMessage logTextStdout

    opts :: ParserInfo Options
    opts = info (parseOptions <**> helper)
           ( fullDesc
          <> progDesc "Toronto Bikeshare CLI and API client."
          <> header "Toronto Bikeshare" )


-- Main application entry point inside the 'App' monad environment.
appMain :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m) => Options -> m ()
appMain options = do
  log I $ "Starting Toronto Bikeshare CLI with verbosity '" <> Text.pack (show (logLevel options)) <> "'."
  -- Dispatch to appropriate command.
  case optCommand options of
    (Poll p)      -> dispatchDatabase options >> dispatchPoll p
    (Query q)     -> dispatchDatabase options >> dispatchQuery q
    QueryApi      -> log E "Not implemented."
    (Events e)    -> dispatchDatabase options >> dispatchEvents (optEventsSubcommand e)
    (DebugMisc d) -> dispatchDatabase options >> dispatchDebug d
    (Reset _)     -> void (dispatchDatabase options)

-- Convert CLI options to a logging severity.
logLevel :: Options -> Severity
logLevel options = case length (optVerbose options) of
  0 -> Warning
  1 -> Info
  2 -> Debug
  3 -> Debug
  _ -> Debug
