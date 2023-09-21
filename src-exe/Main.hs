module Main
     ( main
     ) where

import qualified API.Poll       as P

import           Database.Utils


main :: IO ()
main = do
  -- Setup the database.
  conn <- setupDatabaseName dbnameProduction

  -- Run API poller method.
  P.pollClient conn
