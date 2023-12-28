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

module Database.BikeShare.Schema.V001.Diagnostics
     ( Diagnostics
     , DiagnosticsId
     , DiagnosticsT (..)
     , PrimaryKey (DiagnosticsId)
       -- Lenses
     , diagnosticId
     , diagnosticTime
     ) where


import           Control.Lens

import           Data.Int
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend ( SqlSerial )

-- Parameterized depending on which table the entry is for.
data DiagnosticEntry where
  InfoTableDiagnostic     :: InfoDiagnosticEntry -> DiagnosticEntry
  StatusTableDiagnostic   :: StatusDiagnosticEntry -> DiagnosticEntry
  deriving (Generic, Show)

data InfoDiagnosticEntry where
  InfoDiagnosticEntry :: { } -> InfoDiagnosticEntry
  deriving (Generic, Show)

data StatusDiagnosticEntry where
  StatusDiagnosticEntry :: { } -> StatusDiagnosticEntry
  deriving (Generic, Show)


-- | Declare a (Beam) table for the 'Diagnostics' type
data DiagnosticsT f where
  Diagnostics :: { _diagnosticId       :: Columnar f (SqlSerial Int32)
                 , _diagnosticTime     :: Columnar f UTCTime
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
diagnosticTime :: Lens' (DiagnosticsT f) (C f UTCTime)

Diagnostics (LensFor diagnosticId) _    = tableLenses
Diagnostics _ (LensFor diagnosticTime)  = tableLenses
