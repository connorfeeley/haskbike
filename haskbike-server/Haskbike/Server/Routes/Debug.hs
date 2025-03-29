{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for the parent debug API. Reexports debug sub-API.

module Haskbike.Server.Routes.Debug
     ( DebugAPI (..)
     , module Haskbike.Server.API.Debug.DebugAPI
     , ParentDebugAPI (..)
     ) where

import           GHC.Generics                           ( Generic )

import           Haskbike.Server.API.Debug.DebugAPI
import           Haskbike.Server.Page.Debug.LandingPage

import           Servant
import           Servant.HTML.Lucid


-- | Parent of debugging API endpoints.
data ParentDebugAPI mode where
  ParentDebugAPI ::
    -- Debug landing page.
    { debugPage     :: mode :- "debug" :> Get '[HTML] DebugLandingPage
    -- Rest of debug API.
    , debugApi      :: mode :- "debug" :> NamedRoutes DebugAPI
    } -> ParentDebugAPI mode
  deriving stock Generic

