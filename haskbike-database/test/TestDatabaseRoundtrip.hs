-- |

module TestDatabaseRoundtrip
     ( unit_roundtripStationInformation
     , unit_roundtripStationStatusVehicleTypes
     ) where

import           Control.Lens                                ( (^.) )
import           Control.Monad                               ( forM_, void )

import           Data.List                                   ( sortOn )
import qualified Data.Map                                    as Map

import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.StationStatus                  as AT
import qualified Haskbike.API.VehicleType                    as AT
import           Haskbike.Database.Operations
import qualified Haskbike.Database.Tables.StationInformation as DB
import           Haskbike.Database.Tables.StationStatus      ( fromBeamStationStatusToJSON, vehicleTypesAvailableAstro,
                                                               vehicleTypesAvailableBoost, vehicleTypesAvailableChloe,
                                                               vehicleTypesAvailableCosmo, vehicleTypesAvailableEfit,
                                                               vehicleTypesAvailableEfitG5, vehicleTypesAvailableIconic,
                                                               vehicleTypesAvailableMetro )
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


-- | Insert a status with non-zero counts for every known vehicle type and
-- assert each count is preserved through the DB roundtrip. Catches schema
-- regressions where a column is missing, mis-named, or wired to the wrong field.
unit_roundtripStationStatusVehicleTypes :: IO ()
unit_roundtripStationStatusVehicleTypes = withTempDbM Silent setupTestDatabase $ do
  infoResp <- liftIO $ getDecodedFileInformation "test/json/station_information-1.json"
  void $ insertStationInformation (map (_respLastUpdated infoResp, ) (_respData infoResp))

  -- Pick the first station so the status FK resolves.
  let stationId :: Int = AT.infoStationId (head (_respData infoResp))
      vtaCounts =
        [ (AT.Boost,  1)
        , (AT.Iconic, 2)
        , (AT.EFit,   3)
        , (AT.EFitG5, 4)
        , (AT.CHLOE,  5)
        , (AT.Cosmo,  6)
        , (AT.Astro,  7)
        , (AT.Metro,  8)
        ]
      apiStatus = sampleStatus stationId vtaCounts

  inserted <- insertStationStatus [apiStatus]
  liftIO $ assertEqual "Inserted status row count" 1 (length inserted)

  let beamRow      = head inserted
      readBack vta = beamRow ^. vta
  liftIO $ do
    assertEqual "Boost count preserved"   1 (fromIntegral (readBack vehicleTypesAvailableBoost))
    assertEqual "Iconic count preserved"  2 (fromIntegral (readBack vehicleTypesAvailableIconic))
    assertEqual "EFit count preserved"    3 (fromIntegral (readBack vehicleTypesAvailableEfit))
    assertEqual "EFitG5 count preserved"  4 (fromIntegral (readBack vehicleTypesAvailableEfitG5))
    assertEqual "CHLOE count preserved"   5 (fromIntegral (readBack vehicleTypesAvailableChloe))
    assertEqual "Cosmo count preserved"   6 (fromIntegral (readBack vehicleTypesAvailableCosmo))
    assertEqual "Astro count preserved"   7 (fromIntegral (readBack vehicleTypesAvailableAstro))
    assertEqual "Metro count preserved"   8 (fromIntegral (readBack vehicleTypesAvailableMetro))

  -- Beam → API roundtrip must include every type with the same count.
  let apiRoundtrip = fromBeamStationStatusToJSON beamRow
      lookupCount vt = maybe 0 AT.vehicleTypeCnt (Map.lookup vt (AT._statusVehicleTypesAvailable apiRoundtrip))
  liftIO $ forM_ vtaCounts $ \(vt, expected) ->
    assertEqual ("API roundtrip count for " <> show vt) expected (lookupCount vt)


-- | Build an 'AT.StationStatus' for a given station, with the supplied vehicle-type counts.
sampleStatus :: Int -> [(AT.TorontoVehicleType, Int)] -> AT.StationStatus
sampleStatus sid vtaCounts =
  AT.StationStatus
    { AT._statusStationId             = sid
    , AT._statusNumBikesAvailable     = sum (map snd vtaCounts)
    , AT._statusNumBikesDisabled      = 0
    , AT._statusNumDocksAvailable     = 0
    , AT._statusNumDocksDisabled      = 0
    , AT._statusLastReported          = Just (read "2026-01-01 00:00:00 UTC")
    , AT._statusIsChargingStation     = True
    , AT._statusStatus                = AT.InService
    , AT._statusIsInstalled           = True
    , AT._statusIsRenting             = True
    , AT._statusIsReturning           = True
    , AT._statusTraffic               = Nothing
    , AT._statusVehicleDocksAvailable = []
    , AT._statusVehicleTypesAvailable =
        Map.fromList [ (vt, AT.VehicleType vt cnt) | (vt, cnt) <- vtaCounts ]
    }
