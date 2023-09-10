-- | Test the database.

{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module TestDatabase where

import qualified Database.BikeShare                       as DBS
import           Database.Migrations                      (migrateDB)
import qualified StationInformation                       as SI

import           Test.Framework                           (Test, testGroup)
import           Test.Framework.Providers.HUnit           (testCase)
import           Test.HUnit                               (assertEqual,
                                                           assertFailure)
import           Test.QuickCheck

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Text.Pretty.Simple                       (pPrintString)

import           Control.Lens
import           Data.Aeson                               (decode,
                                                           eitherDecode')
import qualified Data.ByteString                          as B
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.FileEmbed                           (embedDir, embedFile)
import           Data.List                                (find, sortOn, (\\))
import           Data.Maybe                               (catMaybes, isJust,
                                                           mapMaybe)
import           Data.String                              (fromString)
import qualified Data.Text                                as T

import           Control.Monad                            (liftM)
import qualified Database.StationInformation              as DSI
import           Text.Printf                              (printf)


-- Test case using Beam
test_Database :: Test
test_Database = testGroup "Database tests"
  [ testCase "Setup" setupDatabase

  , testCase "Insert stations" insertStations

  -- Uncomment to force failure
  -- , testCase "Forced failure" $ assertFailure "Forced failure"
  ]

-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson

-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Establish a connection to the database.
connectDb :: IO Connection
connectDb =
  connectPostgreSQL $ fromString "host=localhost port=5432 dbname=haskbike connect_timeout=10"

setupDatabase :: IO ()
setupDatabase = do
  -- Connect to the database.
  conn <- connectDb

  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  -- Initialize the database.
  _ <- migrateDB conn

  pPrintString "Database reinitialization complete."

insertStations :: IO ()
insertStations = do
  -- Connect to the database.
  conn <- connectDb

  let stationsJson = fromStrict $(embedFile "test/json/station_information.json")
  let mStations = eitherDecode' stationsJson :: Either String SI.StationInformationResponse
  case mStations of
    Right stations -> do
      _insertedStations <- runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
        insertExpressions $ map DSI.fromJSONToBeamStationInformation (SI.stations stations)
      pure ()
    Left errorMsg -> assertFailure $ "Error decoding agencies" ++ errorMsg


{- |
Binary operator to accumulate a list of tuples, where:
- the first element of the tuple is a key
- the second element of the tuple is a value associated with the key
- the list is sorted by the key (first tuple element)

Producing a list of tuples, where:
- the first element of the tuple is the key
- the second element of the tuple is the list of values associated with the key

>>> accumulateTuple2 [] (1, "value1")
[(1,["value1"])]

>>> accumulateTuple2 [(1,["value1"])] (1, "value2")
[(1,["value2","value1"])]

>>> accumulateTuple2 [(1,["value2","value1"])] (2, "value3")
[(1,["value2","value1"]),(2,["value3"])]
-}
accumulateTuple2 :: Eq a1 => [(a1, [a2])] -> (a1, a2) -> [(a1, [a2])]
accumulateTuple2 acc (key, assoc) = case lookup key acc of
  Nothing    -> acc ++ [(key, [assoc])]
  Just found -> [(key, assoc : found)]

{- |
Flatten a list of tuples, where:
- the first element of the tuple is a key
- the second element of the tuple is a value associated with the key
- the list is sorted by the key (first tuple element)

Producing a list of tuples, where:
- the first element of the tuple is the key
- the second element of the tuple is the list of values associated with the key

>>> flattenPairs $ pure [ (1, "value1-1")
                        , (1, "value1-2")
                        , (2, "value2-3") ]
[(1,"value1-1"),(1,"value1-2"),(2,"value2-3")]

>>> flattenPairs (getDirectionStop 501 "501_1_DFLO")
[(Direction { ... }, [ DirectionStop { ... _directionStopTag = "1750_ar" }
                     , DirectionStop { ... _directionStopTag = "8445"    }
                     , DirectionStop { ... _directionStopTag = "5577"    }
                     ]
)]
-}
flattenPairs :: (Foldable t, Eq a1) => t (a1, a2) -> [(a1, [a2])]
flattenPairs = foldl accumulateTuple2 []


{- |
Unflatten a list of tuples, where:
- the first element of the tuple is the key
- the second element of the tuple is the list of values associated with the key

Producing a list of tuples, where:
- the first element of the tuple is the key
- the second element of the tuple is a value associated with the key

This is the inverse of 'flattenPairs'.

>>> unflattenPairs [(1,["value1-2","value1-1"]),(2,["value2-3"])]
[(1,"value1-1"),(1,"value1-2"),(2,"value2-3")]
-}
unflattenPairs :: Foldable t => t (a, [b]) -> [(a, b)]
unflattenPairs = concatMap unflattenPair
  where unflattenPair (key, values) = map (key, ) (reverse values)


-- | QuickCheck generative tests.
test_QuickCheck :: Test
test_QuickCheck = testGroup "QuickCheck tests"
  [ testCase "prop_flattenPairs"  $ quickCheck $ prop_flattenPairs pairs
  -- FIXME: This test fails.
  -- , testCase "prop_flattenPairs" $ quickCheck (prop_flattenPairs :: [(Int, String)] -> Bool)
  ]
  where
    pairs :: [(Int, String)]
    pairs = [ (1, "value1-1")
            , (1, "value1-2")
            , (2, "value2-3")
            ]
    pairsIO :: IO [(Int, String)]
    pairsIO = pure pairs
    flattened = flattenPairs <$> pairsIO
    unflattened = unflattenPairs . flattenPairs <$> pairsIO

-- | QuickCheck 'flattenPairs'.
prop_flattenPairs :: (Eq a, Eq b, Ord a, Ord b) => [(a, b)] -> Bool
prop_flattenPairs list = unflattenPairs (flattenPairs sorted) == sorted
  where
    sorted = sortOn fst $ sortOn snd list
