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

import           Colog                  ( Message, WithLog, log, pattern D, pattern E, pattern I, pattern W )

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
main = runApp simpleEnv appMain

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m) => m ()
appMain = do
  -- Parse command line options.
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts
  log D $ "Parsed options" <> Text.pack (show options)

  -- Dispatch to appropriate command.
  case optCommand options of
    (Poll p)  -> dispatchDatabase options >>= dispatchPoll
    (Query q) -> dispatchDatabase options >>= dispatchQuery q
    (Reset r) -> void (dispatchDatabase options)
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Toronto Bikeshare CLI and API client."
     <> header "Toronto Bikeshare" )
