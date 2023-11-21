-- | This module provides the pure version information for the package, for use with Nix.

module Version
     ( getCabalVersion
     , getGitVersion
     , version
     ) where

import           Data.Version   ( showVersion )

import           Paths_haskbike ( version )

getCabalVersion :: String
getCabalVersion = showVersion version

getGitVersion :: String
getGitVersion = "@rev@"
