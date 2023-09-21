{-# LANGUAGE TupleSections #-}

module TestMisc where

import           Data.List                  ( sortOn )

import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.QuickCheck      as QC


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
flattenPairs :: (Foldable t, Eq a, Eq b) => t (a, b) -> [(a, [b])]
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
unflattenPairs :: (Foldable t, Eq a, Eq b) => t (a, [b]) -> [(a, b)]
unflattenPairs = concatMap unflattenPair
  where unflattenPair (key, values) = map (key, ) (reverse values)


-- | QuickCheck generative tests.
test_QuickCheck :: TestTree
test_QuickCheck = testGroup "QuickCheck tests"
  [ testProperty "flattenPairs" $ propertyFlattenPairs pairs
  -- FIXME: This test fails.
  , expectFail $ testProperty "flattenPairs" (propertyFlattenPairs :: [(Int, String)] -> Property)
  ]
  where
    pairs :: [(Int, String)]
    pairs = [ (1, "value1-1")
            , (1, "value1-2")
            , (2, "value2-3")
            ]
    pairsIO :: IO [(Int, String)]
    pairsIO = pure pairs
    _flattened = flattenPairs <$> pairsIO
    _unflattened = unflattenPairs . flattenPairs <$> pairsIO

-- | QuickCheck 'flattenPairs'.
testFlattenPairs :: (Eq a, Eq b, Ord a, Ord b) => [(a, b)] -> Bool
testFlattenPairs list = unflattenPairs (flattenPairs sorted) == sorted
  where
    sorted = sortOn fst $ sortOn snd list

-- | Tasty property for QuickCheck 'flattenPairs'.
propertyFlattenPairs :: (Eq a, Eq b, Ord a, Ord b) => [(a, b)] -> Property
propertyFlattenPairs list = True ==> unflattenPairs (flattenPairs sorted) == sorted
  where
    sorted = sortOn fst $ sortOn snd list
