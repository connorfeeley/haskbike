
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Test the database's charging count calculations and docking/undocking counts.

module TestOperations
     ( unit_queryFieldIntegrals
     ) where

import           AppEnv

import           Data.Time

import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit

import           TestDatabase

import           Utils


-- * Test units.

-- | HUnit test to query all charging events.
unit_queryFieldIntegrals :: IO ()
unit_queryFieldIntegrals = do
  _ <- setupTestDatabase
  _ <- initDBWithExportedData

  let variation = StatusVariationQuery (Just 7001) [ EarliestTime (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
                                                   , LatestTime   (UTCTime (fromGregorian 2023 10 31) (timeOfDayToTime midnight))
                                                   ]
  let expected = [ StatusIntegral { intStatusStationId        = 7001
                                , intStatusVariation          = variation
                                , intStatusCapacity           = 23
                                , intStatusTotalSeconds       = 42805
                                , intStatusSecBikesAvailable  = 296528
                                , intStatusSecBikesDisabled   = 496422
                                , intStatusSecDocksAvailable  = 191565
                                , intStatusSecDocksDisabled   = 0
                                , intStatusSecIconicAvailable = 62361
                                , intStatusSecEfitAvailable   = 54744
                                , intStatusSecEfitG5Available = 179423
                                }
                 ]

  integrals <- runWithAppM dbnameTest $ queryIntegratedStatus variation

  assertEqual "Expected values of column integrals" expected integrals
