-- | Typeclass for classes that can

module Haskbike.Database.BeamConvertable
     ( BeamConvertable (..)
     ) where


class BeamConvertable a b where
    convertToBeam :: a -> b
