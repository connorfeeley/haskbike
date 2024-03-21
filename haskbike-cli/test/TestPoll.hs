-- | Test the client functions.
module TestPoll where

import           Control.Monad                ( void )

import           Haskbike.AppEnv
import           Haskbike.CLI.Options         ( PollOptions (..), PopulateStatusChangesOpt (..) )
import qualified Haskbike.CLI.Poll            as Poll
import           Haskbike.Database.BikeShare  ( migrateDB )
import           Haskbike.Database.Test.Utils

import           Prelude                      hiding ( log, unwords )

import           UnliftIO                     ( timeout )


unit_poll :: IO ()
unit_poll = do
  withTempDbM Silent migrateDB doPoll
  where
    pollOpts = PollOptions NeverPopulate

    doPoll :: AppM ()
    doPoll = void $ timeout 1000000 $ do -- Terminate after 1 second
      (void . Poll.pollClient) pollOpts
