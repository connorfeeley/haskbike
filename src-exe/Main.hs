module Main where

import           API.Client              (stationStatus)
import qualified API.Client              as C
import qualified API.Poll                as P
import           API.Types

import           Control.Exception       (Exception (displayException))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Pretty.Simple      (pPrintString)

main :: IO ()
main = do
  -- Run API poller main method.
  P.main
