{-# LANGUAGE TypeApplications #-}

-- | Command-line options.

module Haskbike.CLI.Options
     ( Command (..)
     , module Haskbike.CLI.Options.Database
     , module Haskbike.CLI.Options.Debug
     , module Haskbike.CLI.Options.Events
     , module Haskbike.CLI.Options.Poll
     , module Haskbike.CLI.Options.Query
     , module Haskbike.CLI.Options.Server
     , Options (..)
     , commandParser
     , parseOptions
     ) where

import           Haskbike.CLI.Options.Command
import           Haskbike.CLI.Options.Database
import           Haskbike.CLI.Options.Debug
import           Haskbike.CLI.Options.Events
import           Haskbike.CLI.Options.Poll
import           Haskbike.CLI.Options.Query
import           Haskbike.CLI.Options.Server
import           Haskbike.Database.Utils

import           Options.Applicative

import           Prelude                       hiding ( log )


-- | Top-level options.
data Options where
  Options :: { optVersion         :: !Bool   -- ^ Flag for version (derived from cabal file and git revision).
             , optCommand         :: !Command
             , optVerbose         :: ![Bool] -- ^ Verbosity flags.
             , optLogDatabase     :: !Bool   -- ^ If database queries should be logged.
             , optLogRichOutput   :: !Bool   -- ^ If log output should use rich messages.
             , optLogBuffering    :: !Bool   -- ^ If log output should be buffered.
             , optDatabase        :: !String
             , optEnableMigration :: !Bool
             } -> Options
  deriving (Show)

-- | Parser for 'Options'.
parseOptions :: Parser Options
parseOptions = Options
  <$> versionFlag
  <*> commandParser
  -- Support multiple verbosity flags.
  <*> many verboseFlag
  <*> logDatabase
  <*> logPlain
  <*> logBuffering
  <*> strOption
      ( long "database"
     <> metavar "DATABASE"
     <> showDefault
     <> value dbnameProduction
     <> help "Target database name." )
  <*> switch
      ( long "enable-migrations"
     <> showDefault
     <> help "Perform database migrations." )
  where
    versionFlag = switch
                 (long "version"
               <> short 'V'
               <> help "Print version (from cabal file and git revision).")
    verboseFlag = flag' True -- Active when flag is present.
                 (long "verbose"
               <> short 'v'
               <> help "Output verbosity (pass multiple times for more verbosity)."
               <> showDefault)
    logDatabase = switch
                 (long "log-database"
               <> showDefault
               <> help "Log database queries." )
    logPlain = flag True False
              (long "plain"
            <> short 'p'
            <> help "Plain logging output, instead of rich messages."
            <> showDefault)
    logBuffering = flag True False
             (long "unbuffered"
           <> help "Disable stdout and stderr bufferring."
           <> showDefault)

-- | Top-level commands.
data Command where
  Poll           :: !PollOptions     -> Command
  Query          :: !QueryOptions    -> Command
  QueryApi       :: Command
  Events         :: !EventsOptions   -> Command
  Server         :: !ServerOptions   -> Command
  DebugMisc      :: !DebugCommand    -> Command
  Database       :: !DatabaseCommand -> Command
  deriving (Show)

-- | Parser for 'Command'.
commandParser :: Parser Command
commandParser = hsubparser
  (  command "poll"
    (info (Poll          <$> pollOptionsParser)      (progDesc (commandDesc @PollOptions)))
  <> command "query"
    (info (Query         <$> queryOptionsParser)     (progDesc (commandDesc @QueryOptions)))
  <> command "events"
    (info (Events        <$> eventsOptionsParser)    (progDesc (commandDesc @EventsOptions)))
  <> command "serve"
    (info (Server        <$> serverOptionsParser)    (progDesc (commandDesc @ServerOptions)))
  <> command "debug"
    (info (DebugMisc     <$> debugCommandParser)     (progDesc (commandDesc @DebugCommand)))
  <> command "database"
    (info (Database      <$> databaseCommandParser)  (progDesc (commandDesc @DatabaseCommand)))
  )
