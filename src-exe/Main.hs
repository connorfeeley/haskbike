module Main where

import           API.Client              (stationStatus)
import qualified API.Client              as C
import           API.Types

import           Control.Exception       (Exception (displayException))
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Pretty.Simple      (pPrintString)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  -- C.run
  clientManager <- newManager tlsManagerSettings
  status <- C.runQuery clientManager stationStatus
  case status of
    Left err       -> pPrintString $ displayException err
    Right response -> pPrintString $ show $ head $ status_stations response
