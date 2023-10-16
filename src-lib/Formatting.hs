-- | Formatting utilities.

module Formatting
     ( compactOutputOptionsDarkBg
     , pPrintCompact
     , pShowCompact
     ) where


import qualified Data.Text.Lazy     as TextL

import           Database.Beam

import           Text.Pretty.Simple


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty compactOutputOptionsDarkBg

-- | pShow with compact output.
pShowCompact :: Show a => a -> TextL.Text
pShowCompact = pShowOpt compactOutputOptionsDarkBg

compactOutputOptionsDarkBg :: OutputOptions
compactOutputOptionsDarkBg = defaultOutputOptionsDarkBg { outputOptionsCompact = True
                                                        , outputOptionsCompactParens = True
                                                        }
