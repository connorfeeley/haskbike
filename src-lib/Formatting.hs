-- | Formatting utilities.

module Formatting
     ( compactOutputOptions
     , pPrint
     , pPrintCompact
     , pPrintCompactNoColor
     , pShow
     , pShowCompact
     ) where


import qualified Data.Text.Lazy     as TextL

import           Database.Beam

import           Text.Pretty.Simple


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty (compactOutputOptions defaultOutputOptionsDarkBg)

pPrintCompactNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintCompactNoColor = pPrintOpt CheckColorTty (compactOutputOptions defaultOutputOptionsNoColor)

-- | pShow with compact output.
pShowCompact :: Show a => a -> TextL.Text
pShowCompact = pShowOpt (compactOutputOptions defaultOutputOptionsDarkBg)

compactOutputOptions :: OutputOptions -> OutputOptions
compactOutputOptions opts = opts { outputOptionsCompact = True
                                 , outputOptionsCompactParens = False
                                 , outputOptionsPageWidth = 60
                                 , outputOptionsIndentAmount = 4
                                 }
