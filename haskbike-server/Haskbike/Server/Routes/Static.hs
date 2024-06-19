{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

module Haskbike.Server.Routes.Static
     ( StaticAPI (..)
     ) where

import           GHC.Generics ( Generic )

import           Servant


-- * API to serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic
