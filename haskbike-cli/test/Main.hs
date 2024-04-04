-- |

module Main
     ( main
     ) where

import           Test.Tasty

import qualified TestPoll


main :: IO ()
main = defaultMain $
  testGroup "haskbike-client tests"
      [ TestPoll.tests
      ]
