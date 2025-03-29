{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the version information for the package.

module Haskbike.Version
     ( getCabalVersion
     , getGitHash
     , getGitVersion
     , version
     ) where

import qualified Data.Text           as T
import           Data.Version        ( showVersion )

import           GitHash

import           Paths_haskbike_core ( version )

gi :: GitInfo
gi = $$tGitInfoCwd

getCabalVersion :: T.Text
getCabalVersion = (T.pack . showVersion) version

getGitVersion :: T.Text
getGitVersion = tag <> " " <> if dirty then "(dirty)" else "" <> " " <> date
  where
    tag   = T.pack $ giTag        gi
    date  = T.pack $ giCommitDate gi
    dirty = giDirty      gi

getGitHash :: T.Text
getGitHash = (T.pack . giHash) gi
