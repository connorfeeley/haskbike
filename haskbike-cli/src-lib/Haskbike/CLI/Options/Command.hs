{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Typeclasses for command line options.

module Haskbike.CLI.Options.Command
     ( HasCommandDesc (..)
     ) where


class HasCommandDesc a where
  commandDesc :: String
