-- | Formatting utilities.

module Formatting
     ( pPrintCompact
     ) where


import           Database.Beam

import           Text.Pretty.Simple


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty pPrintCompactOpt
  where
    pPrintCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }
