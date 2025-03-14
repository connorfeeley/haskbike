-- |

module Haskbike.API.VehicleType where

import           Data.Aeson
import           Data.Attoparsec.Text ( Parser, choice, parseOnly, string )
import           Data.Char            ( toUpper )
import           Data.Either          ( fromRight )
import           Data.Functor         ( ($>) )
import           Data.List            ( find )
import qualified Data.Map             as Map
import qualified Data.Text            as T

import           GHC.Generics         ( Generic )


-- | A type representing a BikeShare station's vehicle type status.
data VehicleType where
  VehicleType :: { vehicleTypeId  :: TorontoVehicleType
                 , vehicleTypeCnt :: Int
                 } -> VehicleType
  deriving (Show, Generic, Eq, Ord)

instance ToJSON VehicleType where
  toJSON types_available =
    object [ "vehicle_type_id" .= show (vehicleTypeId types_available)
           , "count"           .= vehicleTypeCnt      types_available
           ]
instance FromJSON VehicleType where
  parseJSON = withObject "VehicleType" $ \v -> VehicleType
    <$> v .: "vehicle_type_id"
    <*> v .: "count"

data TorontoVehicleType where
  Fit    :: TorontoVehicleType
  Boost  :: TorontoVehicleType
  Iconic :: TorontoVehicleType
  EFit   :: TorontoVehicleType
  EFitG5 :: TorontoVehicleType
  CHLOE  :: TorontoVehicleType
  deriving (Generic, Eq, Ord)

instance Show TorontoVehicleType where
  show Fit    = "FIT"
  show Boost  = "BOOST"
  show Iconic = "ICONIC"
  show EFit   = "EFIT"
  show EFitG5 = "EFIT G5"
  show CHLOE  = "CHLOE"

-- | Read instance for 'TorontoVehicleType' (case-insensitive).
instance Read TorontoVehicleType where
  readsPrec _ = fromRight [] . parseOnly parser . T.pack . map toUpper
    where
    parser :: Parser [(TorontoVehicleType, String)]
    parser = choice
      [ string "FIT"      $> [(Fit,    "")]
      , string "BOOST"    $> [(Boost,  "")]
      , string "ICONIC"   $> [(Iconic, "")]
      , string "EFIT"     $> [(EFit,   "")]
      , string "EFIT G5"  $> [(EFitG5, "")]
      , string "CHLOE"    $> [(CHLOE,  "")]
      ]

instance ToJSON TorontoVehicleType where
  toJSON Fit    = String (T.pack "FIT")
  toJSON Boost  = String (T.pack "BOOST")
  toJSON Iconic = String (T.pack "ICONIC")
  toJSON EFit   = String (T.pack "EFIT")
  toJSON EFitG5 = String (T.pack "EFIT G5")
  toJSON CHLOE  = String (T.pack "CHLOE")

instance FromJSON TorontoVehicleType where
  parseJSON = withText "TorontoVehicleType" $ \t -> case t of
     "FIT"     -> return Fit
     "BOOST"   -> return Boost
     "ICONIC"  -> return Iconic
     "EFIT"    -> return EFit
     "EFIT G5" -> return EFitG5
     "CHLOE"   -> return CHLOE
     _         -> fail ("Invalid TorontoVehicleType: " ++ show t)

-- | List of 'VehicleType' to Map.
listToMap :: [VehicleType] -> Map.Map TorontoVehicleType VehicleType
listToMap = Map.fromList . map elemToKV

elemToKV :: VehicleType -> (TorontoVehicleType, VehicleType)
elemToKV vt = (vehicleTypeId vt, vt)

mapToList :: Map.Map TorontoVehicleType VehicleType -> [VehicleType]
mapToList = map kvToElem . Map.toList

kvToElem :: (TorontoVehicleType, VehicleType) -> VehicleType
kvToElem = snd

-- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
typeInList :: Foldable t => TorontoVehicleType -> t VehicleType -> Maybe VehicleType
typeInList vehicleType = find (\x -> vehicleTypeId x == vehicleType)

-- | Find the corresponding value in the list.
findByType :: (Num b, Foldable t) => TorontoVehicleType -> t VehicleType -> b
findByType tvt xs = fromIntegral $ maybe 0 vehicleTypeCnt (typeInList tvt xs)

numBoost, numIconic, numEfit, numEfitG5, numChloe :: Num a => [VehicleType] -> a
numBoost  = findByType Boost
numIconic = findByType Iconic
numEfit   = findByType EFit
numEfitG5 = findByType EFitG5
numChloe  = findByType CHLOE
