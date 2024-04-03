-- | Test the client functions.

module TestPoll
     ( tests
     ) where

import           Control.Monad                ( void )
import           Control.Monad.Reader         ( runReaderT )

import           Haskbike.API.MockServer      ( mockServerBaseUrl, runMockServer )
import           Haskbike.AppEnv
import           Haskbike.CLI.Options         ( PollOptions (..), PopulateStatusChangesOpt (..) )
import qualified Haskbike.CLI.Poll            as Poll
import           Haskbike.Database.BikeShare  ( migrateDB )
import           Haskbike.Database.Test.Utils

import           Prelude                      hiding ( log, unwords )

import           Test.Tasty
import           Test.Tasty.HUnit

import           UnliftIO                     ( concurrently, liftIO, timeout )


tests :: TestTree
tests = testGroup "Polling tests"
  [ testCase "Poll API"      poll
  , testCase "Poll mock API" pollMock
  ]


poll :: IO ()
poll = do
  withTempDbM Silent migrateDB doPoll
  where
    pollOpts = PollOptions NeverPopulate

    doPoll :: AppM ()
    doPoll = void $ timeout (5 * 1000000) $ -- Terminate after 5 seconds
             Poll.pollClient pollOpts

pollMock :: IO ()
pollMock = do
  withTempDbM Silent migrateDB $ do
    env <- ask
    liftIO $ runReaderT (unAppM doPoll) (env { envBaseUrl = mockServerBaseUrl })
  where
    pollOpts = PollOptions NeverPopulate

    doPoll :: AppM ()
    doPoll = void $ timeout (5 * 1000000) $ -- Terminate after 5 seconds
             concurrently (runMockServer 8082)
                          (Poll.pollClient pollOpts)
