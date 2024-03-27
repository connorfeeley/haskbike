-- | Options for the polling commands.

module Haskbike.CLI.Options.Poll
     ( PollOptions (..)
     , PopulateStatusChangesOpt (..)
     , pollOptionsParser
     , populateStatusChangesParser
     ) where

import qualified Data.Attoparsec.Text as A
import           Data.Either          ( fromRight )
import           Data.Functor         ( ($>) )
import qualified Data.Text            as T

import           Options.Applicative


-- | Options for the 'Poll' command.
data PollOptions where
  PollOptions :: { optPollPopulateStatusChanges :: !PopulateStatusChangesOpt
                 } -> PollOptions
  deriving (Show)

-- | Parser for 'PollOptions'.
pollOptionsParser :: Parser PollOptions
pollOptionsParser = PollOptions <$> populateStatusChangesParser

-- | Valid options for populating the 'station status changes' table.
data PopulateStatusChangesOpt where
  AlwaysPopulate :: PopulateStatusChangesOpt
  AutoPopulate   :: PopulateStatusChangesOpt
  NeverPopulate  :: PopulateStatusChangesOpt
  deriving (Enum, Bounded, Eq)

instance Show PopulateStatusChangesOpt where
  show AlwaysPopulate = "always"
  show AutoPopulate   = "auto"
  show NeverPopulate  = "never"

-- | Read instance for 'PopulateStatusChangesOpt'.
instance Read PopulateStatusChangesOpt where
  readsPrec _ = fromRight [] . A.parseOnly parser . T.pack
    where
    parser :: A.Parser [(PopulateStatusChangesOpt, String)]
    parser = A.choice
      [ A.asciiCI "always" $> [(AlwaysPopulate, "")]
      , A.asciiCI "auto"   $> [(AutoPopulate,   "")]
      , A.asciiCI "never"  $> [(NeverPopulate,  "")]
      ]

-- | optparse-applicative 'Parser' instance for 'PopulateStatusChangesOpt'.
populateStatusChangesParser :: Parser PopulateStatusChangesOpt
populateStatusChangesParser = -- Relies on 'Read' instance.
  option auto
    ( long "populate-status-changes"
   <> help ("Populate the 'station status changes' table with data from the 'station status' table. Allowed values: " <> show allowedValues)
   <> value AutoPopulate
   <> showDefault
    )
  where
    allowedValues :: [PopulateStatusChangesOpt]
    allowedValues = [minBound..]
