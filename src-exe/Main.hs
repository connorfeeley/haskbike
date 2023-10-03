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

import           Colog                  ( Message, Severity (..), WithLog, log, pattern D, pattern E, pattern I,
                                          pattern W )

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

  runApp (mainEnv (logLevel options)) (appMain options)
  where
    opts :: ParserInfo Options
    opts = info (parseOptions <**> helper)
           ( fullDesc
          <> progDesc "Toronto Bikeshare CLI and API client."
          <> header "Toronto Bikeshare" )


-- Main application entry point inside the 'App' monad environment.
appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m) => Options -> m ()
appMain options = do
  log I $ "Starting Toronto Bikeshare CLI with verbosity '" <> Text.pack (show (logLevel options)) <> "'."
  -- Dispatch to appropriate command.
  case optCommand options of
    (Poll p)  -> dispatchDatabase options >>= dispatchPoll p
    (Query q) -> dispatchDatabase options >>= dispatchQuery q
    (Reset r) -> void (dispatchDatabase options)

-- Convert CLI options to a logging severity.
logLevel :: Options -> Severity
logLevel options = case length (optVerbose options) of
  0 -> Warning
  1 -> Info
  2 -> Debug
  3 -> Debug
  _ -> Debug
