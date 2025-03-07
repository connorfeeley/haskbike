-- | Formatting utilities.

module Text.Pretty.Simple.Extras
     ( compactOutputOptions
     , pPrint
     , pPrintCompact
     , pPrintCompactNoColor
     , pShow
     , pShowCompact
     ) where


import qualified Data.Text.Lazy     as TextL

import           Text.Pretty.Simple

import           UnliftIO           ( MonadIO )


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty (compactOutputOptions defaultOutputOptionsDarkBg)
{-# INLINE pPrintCompact #-}

pPrintCompactNoColor :: (MonadIO m, Show a) => a -> m ()
pPrintCompactNoColor = pPrintOpt CheckColorTty (compactOutputOptions defaultOutputOptionsNoColor)
{-# INLINE pPrintCompactNoColor #-}

-- | pShow with compact output.
pShowCompact :: Show a => a -> TextL.Text
pShowCompact = pShowOpt (compactOutputOptions defaultOutputOptionsDarkBg)
{-# INLINE pShowCompact #-}

compactOutputOptions :: OutputOptions -> OutputOptions
compactOutputOptions opts = opts { outputOptionsCompact = True
                                 , outputOptionsCompactParens = False
                                 , outputOptionsPageWidth = 60
                                 , outputOptionsIndentAmount = 4
                                 }
{-# INLINE compactOutputOptions #-}
