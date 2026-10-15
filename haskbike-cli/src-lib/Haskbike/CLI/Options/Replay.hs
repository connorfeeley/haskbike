-- | Options for the 'replay' command.

module Haskbike.CLI.Options.Replay
     ( ReplayOptions (..)
     , replayOptionsParser
     ) where

import           Data.Int                          ( Int32 )
import qualified Data.Text                         as T
import           Data.Time                         ( UTCTime )

import           Haskbike.CLI.Options.Command
import           Haskbike.Database.EndpointQueried

import           Options.Applicative

import           Servant.API                       ( parseUrlPiece )


-- | Options for the 'Replay' command.
data ReplayOptions where
  ReplayOptions :: { optReplayEndpoint :: Maybe EndpointQueried
                   , optReplaySince    :: Maybe UTCTime
                   , optReplayUntil    :: Maybe UTCTime
                   , optReplayLimit    :: Maybe Integer
                   , optReplayId       :: Maybe Int32
                   , optReplayDryRun   :: Bool
                   } -> ReplayOptions
  deriving (Eq, Show)

instance HasCommandDesc ReplayOptions where
  commandDesc = "Re-process failed API queries stored in the 'queries' table."

-- | Parser for 'ReplayOptions'.
replayOptionsParser :: Parser ReplayOptions
replayOptionsParser = ReplayOptions
  <$> optional (option (eitherReader parseEndpoint)
      ( long "endpoint"
     <> metavar "ENDPOINT"
     <> help "Restrict to a single endpoint (station-information | station-status | system-information)." ))
  <*> optional (option auto
      ( long "since"
     <> metavar "UTCTIME"
     <> help ("Restrict to queries logged at or after this time (UTC), inclusive; filters on "
           <> "queries.time (>=). (format: YYYY-MM-DD HH:MM:SS UTC)") ))
  <*> optional (option auto
      ( long "until"
     <> metavar "UTCTIME"
     <> help ("Restrict to queries logged at or before this time (UTC), inclusive; filters on "
           <> "queries.time (<=). (format: YYYY-MM-DD HH:MM:SS UTC)") ))
  <*> optional (option auto
      ( long "limit"
     <> metavar "N"
     <> help "Limit the number of query logs replayed." ))
  <*> optional (option auto
      ( long "id"
     <> metavar "QUERY_LOG_ID"
     <> help "Restrict to a single query log ID." ))
  <*> switch
      ( long "dry-run"
     <> help "Report which query logs would be replayed without inserting or marking them replayed." )
  where
    parseEndpoint :: String -> Either String EndpointQueried
    parseEndpoint piece = case parseUrlPiece (T.pack piece) of
      Left err -> Left (T.unpack err)
      Right ep -> Right ep
