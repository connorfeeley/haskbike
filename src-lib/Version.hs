{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the version information for the package.

module Version
     ( getCabalVersion
     , getGitHash
     , getGitVersion
     , version
     ) where

import           Data.Version   ( showVersion )

import           GitHash

import           Paths_haskbike ( version )

gi :: GitInfo
gi = $$tGitInfoCwd

getCabalVersion :: String
getCabalVersion = showVersion version

getGitVersion :: String
getGitVersion = tag <> " " <> if dirty then "(dirty)" else "" <> " " <> date
  where
    tag   = giTag        gi
    date  = giCommitDate gi
    dirty = giDirty      gi

getGitHash :: String
getGitHash = giHash gi
