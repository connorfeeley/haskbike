module Main where

import qualified Client as C
import Client (stationStatus)
import StationStatus (StationStatusResponse(..))

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Pretty.Simple (pPrintString)
import Control.Exception (Exception(displayException))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  -- C.run
  clientManager <- newManager tlsManagerSettings
  status <- C.runQuery clientManager stationStatus
  case status of
    Left err -> pPrintString $ displayException err
    Right response -> pPrintString $ show $ head $ stations response
