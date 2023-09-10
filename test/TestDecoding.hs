{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module TestDecoding where

import qualified StationInformation   as SI
import qualified StationStatus        as SS

import           Test.Tasty.HUnit

import           Data.Aeson           (FromJSON, eitherDecode)

import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as BL

import           Data.FileEmbed       (embedDir)

import qualified Data.Maybe           as Maybe


-- | Decode a ByteString into a value.
decodeByteString :: FromJSON a => BL.ByteString -> Either String a
decodeByteString = eitherDecode

-- | Test parsing of a ByteString.
testParse :: forall a. FromJSON a => a -> B.ByteString -> IO ()
testParse (_ :: a) bs = do
    result :: Either String a <- return $ decodeByteString $ fromStrict bs
    case result of
        Left err -> assertFailure $ "Error parsing: " ++ err
        Right _  -> return ()

-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson

-- | Create a test case.
buildTestCase :: FromJSON a => a -> String -> IO ()
buildTestCase (_ :: a) file = testParse (undefined :: a) (Maybe.fromMaybe "" $ lookupJson file)

-- | Test decoding of JSON files.

unit_stationInformation :: IO ()
unit_stationInformation = buildTestCase (undefined :: SI.StationInformationResponse) "station_information.json"

unit_stationStatus :: IO ()
unit_stationStatus = buildTestCase (undefined :: SS.StationStatusResponse) "station_status.json"
