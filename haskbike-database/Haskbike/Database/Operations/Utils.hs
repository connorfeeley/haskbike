-- |

module Haskbike.Database.Operations.Utils
     ( infoStationIdCond
     , stationIdCond
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )

import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                       ( BeamSqlBackend )
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus

-- | Possible filter condition for station ID.
stationIdCond :: ( HaskellLiteralForQExpr (expr Bool) ~ Bool
                 , SqlEq expr (Columnar f Int32)
                 , Integral a
                 , SqlValable (expr Bool)
                 , SqlValable (Columnar f Int32)
                 , Num (HaskellLiteralForQExpr (Columnar f Int32))
                 )
              => Maybe a
              -> StationStatusT f
              -> expr Bool
stationIdCond (Just stationId') row = (_unInformationStationId . _statusInfoId . _statusCommon) row ==. val_ (fromIntegral stationId')
stationIdCond Nothing           _   = val_ True


infoStationIdCond :: (HaskellLiteralForQExpr (expr Bool) ~ Bool, SqlEq expr (Columnar f Int32), Integral a, SqlValable (expr Bool), SqlValable (Columnar f Int32), Num (HaskellLiteralForQExpr (Columnar f Int32)))
                  => Maybe a -> StationInformationT f -> expr Bool
infoStationIdCond (Just stationId') row = _infoStationId row ==. val_ (fromIntegral stationId')
infoStationIdCond Nothing           _   = val_ True
