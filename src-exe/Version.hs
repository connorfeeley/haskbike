{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the version information for the package.

module Version
     ( getCabalVersion
     , getGitVersion
     , version
     ) where

import           Data.Version   ( showVersion )

import           Fmt

import           GitHash

import           Paths_haskbike ( version )

gi :: GitInfo
gi = $$tGitInfoCwd

getCabalVersion :: String
getCabalVersion = showVersion version

getGitVersion :: String
getGitVersion = format "{} {} ({})" tag (if dirty then ("(dirty)" :: String) else "") date
  where
    tag   = giTag        gi
    date  = giCommitDate gi
    dirty = giDirty      gi