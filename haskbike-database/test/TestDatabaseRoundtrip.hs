-- |

module TestDatabaseRoundtrip
     ( unit_roundtripStationInformation
     ) where

import           Control.Monad                               ( forM_ )

import           Data.List                                   ( sortOn )
import qualified Data.Map                                    as Map

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation             as AT
import           Haskbike.Database.Operations
import qualified Haskbike.Database.Tables.StationInformation as DB
import           Haskbike.Database.Test.Utils

import           Test.Tasty.HUnit

import           UnliftIO


unit_roundtripStationInformation :: IO ()
unit_roundtripStationInformation = withTempDbM Silent setupTestDatabase $ do
  infoResp <- liftIO $ getDecodedFileInformation "test/json/station_information-8.json"

  (selected, updated, inserted) <- insertStationInformation' (map (_respLastUpdated infoResp, ) (_respData infoResp))

  liftIO $ assertEqual "Number of selected station information records" 0   (length selected)
  liftIO $ assertEqual "Number of updated  station information records" 0   (length updated)
  liftIO $ assertEqual "Number of inserted station information records" 732 (length inserted)

  let insertedJson = map DB.fromBeamStationInformationToJSON inserted

  let diff = Map.difference (apiMap (_respData infoResp)) (apiMap insertedJson)

  liftIO $ assertEqual "No difference between length of API map and DB map" 0 ((length . Map.elems) diff)

  let intersected = Map.intersection (apiMap insertedJson) ((apiMap . _respData) infoResp)
  liftIO $ assertEqual "Station information intersection length" 732 (length intersected)

  -- Zip inserted data and API data together and assert each element is equal; done per-element to make errors readable.
  forM_ (zip (sortOn AT.infoStationId insertedJson) ((sortOn AT.infoStationId . _respData) infoResp)) $ \(ins, api) ->
    liftIO $ assertEqual "Station information encoding roundtrip" api ins

apiMap :: [AT.StationInformation] -> Map.Map Int AT.StationInformation
apiMap = Map.fromList . map (\inf -> (AT.infoStationId inf, inf))
