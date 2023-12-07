-- | Typeclass for classes that can

module Database.BikeShare.BeamConvertable
     ( BeamConvertable (..)
     ) where


class BeamConvertable a b where
    convertToBeam :: a -> b
