-- |

module Main
     ( main
     ) where

import           Test.Tasty

import qualified TestPoll

import qualified TestReplay


main :: IO ()
main = defaultMain $
  testGroup "haskbike-client tests"
      [ TestPoll.tests
      , TestReplay.tests
      ]
