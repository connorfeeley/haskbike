-- |

module Haskbike.Database.Operations.Utils
     ( infoStationIdCond
     , infoStationIdCond'
     , stationIdCond
     , stationIdCond'
     ) where

import           Data.Int                                    ( Int32 )

import           Database.Beam

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
stationIdCond (Just stationId) row = (_unInformationStationId . _statusInfoId . _statusCommon) row ==. val_ (fromIntegral stationId)
stationIdCond Nothing           _   = val_ True

stationIdCond' :: ( HaskellLiteralForQExpr (expr SqlBool) ~ Bool
                  , SqlEq expr (Columnar f Int32)
                  , Integral a
                  , SqlValable (expr SqlBool)
                  , SqlValable (Columnar f Int32)
                  , Num (HaskellLiteralForQExpr (Columnar f Int32))
                  )
               => Maybe a -> StationStatusT f -> expr SqlBool
stationIdCond' (Just stationId) row = (_unInformationStationId . _statusInfoId . _statusCommon) row ==?. val_ (fromIntegral stationId)
stationIdCond' Nothing           _   = val_ True


infoStationIdCond :: ( HaskellLiteralForQExpr (expr Bool) ~ Bool
                     , SqlEq expr (Columnar f Int32)
                     , Integral a
                     , SqlValable (expr Bool)
                     , SqlValable (Columnar f Int32)
                     , Num (HaskellLiteralForQExpr (Columnar f Int32))
                     )
                  => Maybe a -> StationInformationT f -> expr Bool
infoStationIdCond (Just stationId') row = _infoStationId row ==. val_ (fromIntegral stationId')
infoStationIdCond Nothing           _   = val_ True


infoStationIdCond' :: ( HaskellLiteralForQExpr (expr SqlBool) ~ Bool
                      , SqlEq expr (Columnar f Int32)
                      , Integral a
                      , SqlValable (expr SqlBool)
                      , SqlValable (Columnar f Int32)
                      , Num (HaskellLiteralForQExpr (Columnar f Int32))
                      )
                   => Maybe a -> StationInformationT f -> expr SqlBool
infoStationIdCond' (Just stationId') row = _infoStationId row ==?. val_ (fromIntegral stationId')
infoStationIdCond' Nothing           _   = val_ True
