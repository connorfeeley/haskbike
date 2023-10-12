-- | Formatting utilities.

module Formatting
     ( pPrintCompact
     , pShowCompact
     ) where


import qualified Data.Text.Lazy     as TextL

import           Database.Beam

import           Text.Pretty.Simple


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty pPrintCompactOpt
  where
    pPrintCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }

-- | pShow with compact output.
pShowCompact :: Show a => a -> TextL.Text
pShowCompact = pShowOpt pShowCompactOpt
  where
    pShowCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }
