{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Robots.txt file.

module Server.RobotsTXT
     ( RobotsAPI (..)
     , robotsHandler
     , robotsTxtHandler
     ) where
import qualified Data.ByteString.Char8  as BSW
import qualified Data.Text              as T

import           GHC.Generics           ( Generic )

import           Servant
import           Servant.Server.Generic

import           ServerEnv

data RobotsAPI mode where
  RobotsAPI ::
    { robotsTxt :: mode :- "robots.txt" :> Get '[OctetStream] BSW.ByteString
    } -> RobotsAPI mode
  deriving stock Generic

robotsHandler :: RobotsAPI (AsServerT ServerAppM)
robotsHandler =  RobotsAPI robotsTxtHandler

robotsTxtHandler :: ServerAppM BSW.ByteString
robotsTxtHandler = pure $ BSW.pack "TEST"
