-- |

module Main
     ( main
     ) where

import           Test.Tasty

import qualified TestClient

import qualified TestDecoding


main :: IO ()
main = defaultMain $
  testGroup "haskbike-client tests"
      [ TestClient.tests
      , TestDecoding.tests
      ]
