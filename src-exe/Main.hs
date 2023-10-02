{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

module Main
     ( appMain
     , main
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           CLI.Database
import           CLI.Options
import           CLI.Poll
import           CLI.Query

import           Colog                  ( Message, WithLog, log, pattern D, pattern E, pattern I, pattern W, Severity (Info, Error, Debug) )

import           Control.Lens
import           Control.Monad          ( unless, void, (<=<) )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )

import           Data.Foldable          ( for_ )
import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Options.Applicative

import           Prelude                hiding ( log )

import           UnliftIO               ( MonadUnliftIO )


main :: IO ()
main = do
  -- Parse command line options.
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts

  runApp (cliEnv (logLevel options)) (appMain options)
  where
    -- Set log level based on command line options.
    logLevel options = if optVerbose options then Debug else Error
    cliEnv sev = simpleEnv { envLogAction = mainLogAction sev }

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m) => Options -> m ()
appMain options = do
  -- Dispatch to appropriate command.
  case optCommand options of
    (Poll p)  -> dispatchDatabase options >>= dispatchPoll
    (Query q) -> dispatchDatabase options >>= dispatchQuery q
    (Reset r) -> void (dispatchDatabase options)

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
       ( fullDesc
      <> progDesc "Toronto Bikeshare CLI and API client."
      <> header "Toronto Bikeshare" )
