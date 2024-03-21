{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Test utilities.

module Utils where

import           Control.Lens
import           Control.Monad                   ( void )

import           Data.Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.FileEmbed                  ( embedDir )
import           Data.Time

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation as AT
import qualified Haskbike.API.StationStatus      as AT
import qualified Haskbike.API.SystemInformation  as AT
import           Haskbike.API.Utils
import           Haskbike.AppEnv

import           Test.Tasty.HUnit

-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson

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
