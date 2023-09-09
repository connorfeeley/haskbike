module Main where

import qualified Client as C

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  C.run
