-- | Options for the query commands.

module Haskbike.CLI.Options.Query
     ( MatchMethod (..)
     , QueryMethod (..)
     , QueryOptions (..)
     , parseStationId
     , parseStationName
     , queryOptionsParser
     , unMatchMethod
     ) where

import           Options.Applicative


-- | Options for the 'Query' command.
data QueryOptions where
  QueryOptions :: { optRefresh :: Bool
                  , optQueryBy :: QueryMethod
                  } -> QueryOptions
  deriving (Show)

data QueryMethod where
  QueryByStationId   :: Int                  -> QueryMethod
  QueryByStationName :: (MatchMethod String) -> QueryMethod
  deriving (Show)

data MatchMethod a where
  ExactMatch    :: a -> MatchMethod a
  PrefixMatch   :: a -> MatchMethod a
  SuffixMatch   :: a -> MatchMethod a
  WildcardMatch :: a -> MatchMethod a
  deriving (Show, Functor)

-- | Unwrap a 'MatchMethod'.
unMatchMethod :: MatchMethod a -> a
unMatchMethod (ExactMatch a)    = a
unMatchMethod (PrefixMatch a)   = a
unMatchMethod (SuffixMatch a)   = a
unMatchMethod (WildcardMatch a) = a


-- | Parser for 'QueryOptions'.
queryOptionsParser :: Parser QueryOptions
queryOptionsParser = QueryOptions
  <$> flag True False (long "no-refresh" <> help "Don't refresh the data from the API.")
  <*> hsubparser (queryByIdParser <> queryByNameParser)
  where
    queryByIdParser   = command "id"   (info (QueryByStationId   <$> parseStationId)   (progDesc "Query by station ID."))
    queryByNameParser = command "name" (info (QueryByStationName <$> parseStationName) (progDesc "Query by station name."))

parseStationId :: Parser Int
parseStationId = argument auto
  ( metavar "STATION_ID"
 <> showDefault
 <> value 7001
 <> help "Station ID to query." )

-- | Parser for variants of 'MatchMethod' parameterized over 'String'.
parseStationName :: Parser (MatchMethod String)
parseStationName = exact <|> prefix <|> suffix <|> wildcard where
  exact = ExactMatch <$> strOption
    ( long "exact"
   <> metavar "STATION_NAME"
   <> help "Query for an exact match of the station name." )
  prefix = PrefixMatch <$> strOption
    ( long "prefix"
   <> metavar "PREFIX"
   <> help "Query stations where STATION_NAME appears at the start of the name." )
  suffix = SuffixMatch <$> strOption
    ( long "suffix"
   <> metavar "SUFFIX"
   <> help "Query stations where STATION_NAME appears at the end of the name." )
  wildcard = WildcardMatch <$> argument str
    ( metavar "STATION_NAME"
   <> help "Query stations where STATION_NAME appears anywhere in the name." )
