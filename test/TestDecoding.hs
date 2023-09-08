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
import           Data.ByteString.Lazy           (fromStrict)
import qualified Data.ByteString.Lazy           as B

import           Data.FileEmbed                 (embedFile)

import qualified StationInformation             as SI

-- | Decode a ByteString into a value.
decodeByteString :: FromJSON a => B.ByteString -> Either String a
decodeByteString = eitherDecode

-- | Test parsing of a ByteString.
testParse :: forall a. FromJSON a => a -> B.ByteString -> IO ()
testParse (_ :: a) bs = do
    result :: Either String a <- return $ decodeByteString bs
    case result of
        Left err -> assertFailure $ "Error parsing: " ++ err
        Right _  -> return ()

-- | Test decoding of JSON files.
test_Decoding :: Test
test_Decoding = testGroup "JSON decoding tests" $
    [ testCase "Station Information" (testParse (undefined :: SI.Station) (fromStrict $(embedFile "test/json/station_information.json")))
    ]
