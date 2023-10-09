{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Database table definition for Haskbike diagnostics.

module Database.BikeShare.Diagnostics
     ( Diagnostics
     , DiagnosticsId
     , DiagnosticsT (..)
     , PrimaryKey (DiagnosticsId)
     ) where

import qualified API.Types                                  as API.T

import           Control.Lens

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import qualified Data.Text                                  as Text
import           Data.Vector                                ( fromList, toList )
import qualified Data.Vector                                as Vector

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                              SqlSerial )
import           Database.Beam.Postgres                     ( Postgres )
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )

import           ReportTime

-- | Declare a (Beam) table for the 'Diagnostics' type
data DiagnosticsT f where
  Diagnostics :: { _diagnosticId       :: Columnar f (SqlSerial Int32)
                 , _diagnosticTime     :: Columnar f ReportTime
                 } -> DiagnosticsT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type Diagnostics = DiagnosticsT Identity
type DiagnosticsId = PrimaryKey DiagnosticsT Identity
deriving instance Show DiagnosticsId
deriving instance Eq DiagnosticsId
deriving instance Show Diagnostics
deriving instance Eq Diagnostics


-- | Inform Beam about the table.
instance Table DiagnosticsT where
  data PrimaryKey DiagnosticsT f = DiagnosticsId (C f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = DiagnosticsId . _diagnosticId

-- | Diagnostics lenses
diagnosticId   :: Lens' (DiagnosticsT f) (C f (SqlSerial Int32))
diagnosticTime :: Lens' (DiagnosticsT f) (C f ReportTime)

Diagnostics (LensFor diagnosticId) _    = tableLenses
Diagnostics _ (LensFor diagnosticTime)  = tableLenses
