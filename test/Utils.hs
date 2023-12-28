{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |

module Utils where

import           API.ResponseWrapper
import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT
import           API.Utils

import           AppEnv

import           Control.Monad                                ( void )

import           Data.Aeson
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Lazy                         as BL
import           Data.FileEmbed                               ( embedDir )
import           Data.Time

import           Database.BikeShare
import           Database.BikeShare.ImportExport
import qualified Database.BikeShare.Tables.StationInformation as DB
import qualified Database.BikeShare.Tables.StationStatus      as DB
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit


-- * Test setup.

-- | Initialize empty database from exported station information and station status JSON.
initDBWithExportedData :: IO ([DB.StationInformation], [DB.StationStatus])
initDBWithExportedData = do
  importDbTestData "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-30_2023-10-30.json"


-- * Manually-created test data.

-- | Manually constructed station information record for testing.
manualStationInformation :: AT.StationInformation
manualStationInformation =
  AT.StationInformation { AT.infoStationId = 7001
                        , AT.infoName = "Wellesley Station Green P"
                        , AT.infoPhysicalConfiguration = AT.ElectricBikeStation
                        , AT.infoLat = 43.66496415990742
                        , AT.infoLon = -79.38355031526893
                        , AT.infoAltitude = Just 0.0
                        , AT.infoAddress = Just "Yonge / Wellesley"
                        , AT.infoCapacity = 23
                        , AT.infoIsChargingStation = True
                        , AT.infoRentalMethods = [AT.Key, AT.TransitCard, AT.CreditCard, AT.Phone]
                        , AT.infoIsValetStation = False
                        , AT.infoIsVirtualStation = False
                        , AT.infoGroups = []
                        , AT.infoObcn = "416-617-9576"
                        , AT.infoNearbyDistance = 500.0
                        , AT.infoBluetoothId = ""
                        , AT.infoRideCodeSupport = True
                        , AT.infoRentalUris = AT.RentalURIs "" "" ""
                        }


manualStatus :: [AT.StationStatus]
manualStatus = map (stationStatusFromSimple baseStatus) manualSimpleStatus


manualSimpleStatus :: [StationStatusSimple]
manualSimpleStatus =
  zipWith (\m statusFunc -> statusFunc (TimeOfDay 0 m 0))
  [1..] -- incrementing minutes
  [ \t -> mkStatusSimple t 0 0 0 0 -- (0) Empty station.
  -- One E-Fit docking and undocking.
  , \t -> mkStatusSimple t 0 1 0 0 -- (1) Dock an E-Fit.
  , \t -> mkStatusSimple t 0 0 0 0 -- (2) Undock an E-Fit.
  -- Charge a bike.
  , \t -> mkStatusSimple t 0 0 0 1 -- (3) A mysterious disabled bike appears. Spooky.
  , \t -> mkStatusSimple t 0 1 0 0 -- (4) The mysterious disabled bike is charged, becoming an E-Fit. Wow.
  -- Charge two bikes.
  , \t -> mkStatusSimple t 0 0 0 1 -- (5) A mysterious disabled bike appears. Our ghost returns.
  , \t -> mkStatusSimple t 0 0 0 2 -- (6) A mysterious disabled bike appears. Our ghost has friends.
  , \t -> mkStatusSimple t 0 1 1 0 -- (7) Both disabled bikes are charged, becoming an E-Fit and an E-Fit G5. Thanks, Shift Transit.
  -- Tricksy hobbits docked a busted bike.
  , \t -> mkStatusSimple t 0 0 0 1 -- (8) A mysterious disabled bike appears. Our ghost is injured.
  , \t -> mkStatusSimple t 0 0 0 0 -- (9) The disabled bike disappears. Suspicous.
  -- Dock a dead E-Fit, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit.
  , \t -> mkStatusSimple t 0 0 0 1 -- (10) Dock a mystery bike.
  , \t -> mkStatusSimple t 0 1 0 1 -- (11) Dock a (charged) E-Fit.
  , \t -> mkStatusSimple t 0 0 0 1 -- (12) Undock a (charged) E-Fit.
  , \t -> mkStatusSimple t 0 0 1 0 -- (13) Mystery bike charged up and became an E-Fit G5.
  , \t -> mkStatusSimple t 1 0 1 0 -- (14) Dock an Iconic.
  ]

-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson


setupTestDatabase :: AppM ()
setupTestDatabase = do
  void dropTables
  void migrateDB


-- | Helper function to decode a JSON file.
decodeFile :: FromJSON a => FilePath -- ^ Path to the JSON file.
           -> IO (Either String a)   -- ^ Decoded value.
decodeFile file = eitherDecode <$> BL.readFile file

{- | Read a file as JSON and decode it into a data type.

The file is located at the given 'FilePath'. If the decoding is successful,
the decoded value is returned. If there is an error decoding the JSON, an
assertion failure with the error message is thrown.
-}
getDecodedFile :: FromJSON a => FilePath -- ^ Path to the JSON file.
                             -> IO a     -- ^ Decoded value.
getDecodedFile filePath = either (assertFailure . ("Error decoding JSON: " ++)) return =<< decodeFile filePath

-- | Helper function to decode 'StationInformation' from a JSON file.
getDecodedFileInformation :: FromJSON (ResponseWrapper [AT.StationInformation])
                          => FilePath                                     -- ^ Path to the JSON file.
                          -> IO (ResponseWrapper [AT.StationInformation]) -- ^ Decoded 'StationInformationReponse'.
getDecodedFileInformation = getDecodedFile

-- | Helper function to decode 'StationStatus' from a JSON file.
getDecodedFileStatus :: FromJSON (ResponseWrapper [AT.StationStatus])
                     => FilePath                                -- ^ Path to the JSON file.
                     -> IO (ResponseWrapper [AT.StationStatus]) -- ^ Decoded 'StationStatusReponse'.
getDecodedFileStatus = getDecodedFile

-- | Helper function to decode 'SystemInformation' from a JSON file.
getDecodedFileSystemInformation :: FromJSON (ResponseWrapper AT.SystemInformation)
                                => FilePath                                  -- ^ Path to the JSON file.
                                -> IO (ResponseWrapper AT.SystemInformation) -- ^ Decoded 'StationStatusReponse'.
getDecodedFileSystemInformation = getDecodedFile
