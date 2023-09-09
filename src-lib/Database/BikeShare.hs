-- | Database schema for BikeShare.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare
        ( module DSI
        -- , module DSS
        , BikeshareDb(..)
        , bikeshareDb
        , bikeshareStationInformation
        -- , bikeshareStationStatus
        ) where

import           Database.Beam

import qualified Database.StationInformation as DSI

-- | Define the database; only containing one table for now.
data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation :: f (TableEntity DSI.StationInformationT)
                 -- , _bikeshareStationStatus      :: f (TableEntity DSS.StationStatusT)
                 } -> BikeshareDb f
  deriving (Generic, Database be)

-- | Description of the database.
bikeshareDb :: DatabaseSettings be BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification {
  _bikeshareStationInformation = setEntityName "station_information" <>
                  modifyTableFields tableModification {
                    DSI._information_id = "station_id"
                  }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  -- (TableLens bikeshareStationStatus)
  = dbLenses
