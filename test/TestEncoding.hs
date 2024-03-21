-- |

module TestEncoding
     ( unit_roundtripStationInformation
     ) where

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation                       as AT

import           AppEnv

import           Control.Monad                                ( forM_ )

import           Data.List                                    ( sortOn )
import qualified Data.Map                                     as Map

import           Database.BikeShare.Operations
import qualified Database.BikeShare.Tables.StationInformation as DB
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit

import           Utils


unit_roundtripStationInformation :: IO ()
unit_roundtripStationInformation = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase

  infoJson <- getDecodedFileInformation "docs/json/2.3/station_information-8.json"

  (selected, updated, inserted) <- runWithAppM dbnameTest $
    insertStationInformation' (_respLastUpdated infoJson) (_respData infoJson)

  assertEqual "Number of selected station information records" 0   (length selected)
  assertEqual "Number of updated  station information records" 0   (length updated)
  assertEqual "Number of inserted station information records" 732 (length inserted)

  let insertedJson = map DB.fromBeamStationInformationToJSON inserted

  let diff = Map.difference (apiMap (_respData infoJson)) (apiMap insertedJson)

  assertEqual "No difference between length of API map and DB map" 0 ((length . Map.elems) diff)

  let intersected = Map.intersection (apiMap insertedJson) ((apiMap . _respData) infoJson)
  assertEqual "Station information intersection length" 732 (length intersected)

  -- Zip inserted data and API data together and assert each element is equal; done per-element to make errors readable.
  forM_ (zip (sortOn AT.infoStationId insertedJson) ((sortOn AT.infoStationId . _respData) infoJson)) $ \(ins, api) ->
    assertEqual "Station information encoding roundtrip" api ins

apiMap :: [AT.StationInformation] -> Map.Map Int AT.StationInformation
apiMap = Map.fromList . map (\inf -> (AT.infoStationId inf, inf))
