{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module TestDecoding where

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertFailure)

import           Data.Aeson                     (FromJSON, eitherDecode)
import           Data.Aeson.BetterErrors        (Parse, ParseError, ParseError',
                                                 asIntegral, asString,
                                                 displayError, displayError',
                                                 eachInArray, eachInObject, key,
                                                 keyMay, keyOrDefault, nth,
                                                 nthMay, nthOrDefault, parse,
                                                 toAesonParser, toAesonParser',
                                                 withString)

import qualified Data.ByteString                as B
import           Data.ByteString.Lazy           (fromStrict)
import qualified Data.ByteString.Lazy           as BL

import           Data.FileEmbed                 (embedDir)

import qualified Data.Maybe                     as Maybe

import qualified StationInformation             as SI
import qualified StationStatus                  as SS

-- | All tests defined in this module.
tests :: [Test]
tests = [ test_Decoding ]

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
buildTestCase :: FromJSON a => a -> String -> String -> Test
buildTestCase (_ :: a) name file = testCase name (testParse (undefined :: a) (Maybe.fromMaybe "" $ lookupJson file))

-- | Test decoding of JSON files.
test_Decoding :: Test
test_Decoding = testGroup "JSON decoding tests" $
    [ buildTestCase (undefined :: SI.StationInformationResponse)    "Station Information"   "station_information.json"
    , buildTestCase (undefined :: SS.StationStatusResponse)         "Station Status"        "station_status.json"
    ]
