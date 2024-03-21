module Main
     ( appMain
     , main
     ) where


import           Haskbike.AppEnv

import           Haskbike.CLI.Database
import           Haskbike.CLI.Debug
import           Haskbike.CLI.Events
import           Haskbike.CLI.Options
import           Haskbike.CLI.Poll
import           Haskbike.CLI.Query
import           Haskbike.CLI.ServeVisualize

import           Colog                     ( LogAction, Severity (..), cmap, fmtMessage, log, logInfo, logTextStdout,
                                             pattern D, pattern E, pattern I, usingLoggerT )

import           Control.Monad             ( unless, when )
import           Control.Monad.Reader      ( runReaderT )

import qualified Data.Text                 as T
import qualified Data.Text                 as Text
import           Data.Text.Lazy            ( toStrict )
import qualified Data.Text.Lazy            as TL
import           Data.Time                 ( getCurrentTimeZone )

import           Database.Beam.Postgres    ( ConnectInfo (connectPassword) )

import           Network.HTTP.Client       ( newManager )
import           Network.HTTP.Client.TLS   ( tlsManagerSettings )

import           Options.Applicative

import           Prelude                   hiding ( log, unwords )

import           System.Exit               ( exitSuccess )
import           System.IO

import           Text.Pretty.Simple.Extras

import           UnliftIO                  ( liftIO )

import           Haskbike.Version


main :: IO ()
main = do
  -- Parse command line options.
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts

  -- Check if version flag was used.
  when (optVersion options) $ do
    printVersionFromCabalAndGit
    exitSuccess

  -- Log options when verbosity is high enough.
  when (length (optVerbose options) >= 3) $ usingLoggerT logStdoutAction $
    log D $ "Options: " <> toStrict (pShowCompact options)

  -- Get the current time zone.
  timeZone <- getCurrentTimeZone

  -- Set up database connection pool.
  connInfo <- mkDbConnectInfo (optDatabase options)
  usingLoggerT logStdoutAction $
    logInfo $ "Using database connection: " <> (TL.toStrict . pShowCompact . obfuscatePassword) connInfo
  connPool <- mkDatabaseConnectionPool connInfo

  -- Create HTTPS client manager.
  clientManager <- liftIO $ newManager tlsManagerSettings

  -- Create the application environment.
  let env = mainEnv (logLevel options) (logDatabase options) (optLogRichOutput options) timeZone connPool clientManager

  -- Disable stdout and stderr bufferring when --unbuffered is set.
  unless (optLogBuffering options) disableOutputBuffering

  -- Run the application.
  runReaderT (unAppM $ appMain options) env
  where
    -- Obfuscate password by replacing with asterisks.
    obfuscatePassword connInfo = (connInfo { connectPassword = map (const '*') (connectPassword connInfo) })

    -- | Log database operations only when '--log-database' is given.
    logDatabase = optLogDatabase

    -- | Logging action for stdout.
    logStdoutAction :: LogAction IO Message
    logStdoutAction = cmap fmtMessage logTextStdout

    printVersionFromCabalAndGit = do
      putStrLn $ "Version: " ++ getCabalVersion
      putStrLn $ "Git revision: " ++ getGitVersion

    opts :: ParserInfo Options
    opts = info (parseOptions <**> helper)
           ( fullDesc
          <> progDesc "Toronto Bikeshare CLI and API client."
          <> header "Toronto Bikeshare" )


-- Main application entry point inside the 'AppM' monad environment.
-- appMain :: (MonadUnliftIO m, MonadCatch m, MonadFail m,  HasServerEnv (ServerEnv m) (ReaderT (ServerEnv m) m),  MonadError S.ServerError m, HasEnv env m,  HasEnv (ServerEnv m) (ReaderT (ServerEnv m) m),  HasEnv    (ServerEnv (ReaderT (ServerEnv m) m))    (ServerAppM (ReaderT (ServerEnv m) m))) => Options -> m ()
appMain :: Options -> AppM ()
appMain options = do
  log I $ "Starting Toronto Bikeshare CLI with verbosity '" <> Text.pack (show (logLevel options)) <> "'."
  log I $ "Version: " <> T.pack getCabalVersion <> " | " <> T.pack getGitVersion

  -- Dispatch to appropriate command.
  dispatchDatabase options
  case optCommand options of
    (Poll p)            -> dispatchPoll p
    (Query q)           -> dispatchQuery q
    QueryApi            -> log E "Not implemented."
    (Events e)          -> dispatchEvents (optEventsSubcommand e)
    (DebugMisc d)       -> dispatchDebug d
    (Reset _)           -> pure ()
    (ServeVisualize sv) -> dispatchVisualize sv

-- Convert CLI options to a logging severity.
logLevel :: Options -> Severity
logLevel options = case length (optVerbose options) of
  0 -> Warning
  1 -> Info
  2 -> Debug
  3 -> Debug
  _ -> Debug


disableOutputBuffering :: IO ()
disableOutputBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
