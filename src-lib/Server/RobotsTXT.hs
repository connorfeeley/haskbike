{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Robots.txt file.

module Server.RobotsTXT
     ( RobotsAPI (..)
     , RobotsDefault (..)
     , RobotsUA (..)
     , makeRobots
     , packRobots
     , robotsHandler
     , robotsTxtHandler
     ) where
import           Control.Monad.Catch    ( MonadCatch, MonadThrow )

import qualified Data.ByteString.Char8  as BSW
import           Data.Maybe             ( catMaybes )

import           GHC.Generics           ( Generic )

import           Servant
import           Servant.Server.Generic

import           ServerEnv

import           UnliftIO

-- * API handler.

data RobotsAPI mode where
  RobotsAPI ::
    { robotsTxt :: mode :- "robots.txt" :> Get '[OctetStream] BSW.ByteString
    } -> RobotsAPI mode
  deriving stock Generic

robotsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadIO m) => RobotsAPI (AsServerT m)
robotsHandler =  RobotsAPI robotsTxtHandler

robotsTxtHandler :: (MonadIO m, MonadThrow m) => m BSW.ByteString
robotsTxtHandler = pure $
  packRobots $ makeRobots AllUA AllAllowed (Just 1) ["debug", "data"]


-- * Types and builders.

data RobotsDefault where
  AllAllowed    :: RobotsDefault
  AllDisallowed :: RobotsDefault

data RobotsUA where
  AllUA   :: RobotsUA
  RobotUA :: String -> RobotsUA

{- Make a simple robots.txt.
  >>> makeRobots AllUA AllAllowed (Just 1) ["debug", "data"]
-}
makeRobots :: RobotsUA -> RobotsDefault -> Maybe Int -> [String] -> [String]
makeRobots userAgent robotsDefault crawlDelay disallowRoutes = catMaybes
  [ (Just . userAgentTxt) userAgent
  , (Just . defaultDisallowTxt) robotsDefault
  , crawlDelayTxt crawlDelay
  ] ++ map disallowTxt disallowRoutes

-- | Pack the lines from 'makeRobots' into an ASCII 'ByteString'.
packRobots :: [String] -> BSW.ByteString
packRobots = BSW.pack . unlines

userAgentTxt :: RobotsUA -> String
userAgentTxt ua = "User-agent: " <> case ua of
  AllUA           -> "*"
  RobotUA robotUA -> robotUA

defaultDisallowTxt :: RobotsDefault -> String
defaultDisallowTxt robotsDefault = "Disallow: " <> case robotsDefault of
  AllAllowed    -> ""
  AllDisallowed -> "/"

disallowTxt :: String -> String
disallowTxt path = "Disallow: /" <> path <> "/"

crawlDelayTxt :: Maybe Int -> Maybe String
crawlDelayTxt = fmap (("Crawl-delay: " <>) . show )
