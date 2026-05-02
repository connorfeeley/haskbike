-- | This module provides the pure version information for the package, for use with Nix.

module Haskbike.Version
     ( getCabalVersion
     , getGitHash
     , getGitVersion
     , version
     ) where

import qualified Data.Text           as T
import           Data.Version        ( showVersion )

import           Paths_haskbike_core ( version )

getCabalVersion :: T.Text
getCabalVersion = (T.pack . showVersion) version

getGitVersion :: T.Text
getGitVersion = "@rev@"

getGitHash :: T.Text
getGitHash = "@rev@"
