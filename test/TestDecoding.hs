module TestDecoding
     ( unit_stationInformation
     , unit_stationStatus
     , unit_systemInformation
     ) where

import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus
import           API.SystemInformation

import           Data.Aeson             ( FromJSON, eitherDecode )
import qualified Data.ByteString        as B
import           Data.ByteString.Lazy   ( fromStrict )
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Maybe             as Maybe

import           Test.Tasty.HUnit

import           Utils


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

-- | Create a test case.
buildTestCase :: FromJSON a => a -> String -> IO ()
buildTestCase (_ :: a) file = testParse (undefined :: a) (Maybe.fromMaybe "" $ lookupJson file)

-- | Test decoding of JSON files.

unit_stationInformation :: IO ()
unit_stationInformation = buildTestCase (undefined :: ResponseWrapper [StationInformation]) "station_information.json"

unit_stationStatus :: IO ()
unit_stationStatus = buildTestCase (undefined :: ResponseWrapper [StationStatus]) "station_status.json"

unit_systemInformation :: IO ()
unit_systemInformation = buildTestCase (undefined :: ResponseWrapper SystemInformation) "system_information.json"
