{-# LANGUAGE AllowAmbiguousTypes #-}

-- |

module Server.Page.SelectionForm
     ( SelectionForm (..)
     , SelectionFormInput (..)
     , formatTimeHtml
     , makeInputField
     ) where

import qualified Data.Text        as T
import           Data.Time
import           Data.Time.Extras

import           Lucid

import           TextShow         ( showt )


-- | Various inputs used in 'SelectionForm'.
data SelectionFormInput where
  StationIdInput :: Maybe Int       -> SelectionFormInput
  TimeInput      :: Maybe LocalTime -> SelectionFormInput
  SubmitInput    :: T.Text          -> SelectionFormInput

instance ToHtml SelectionFormInput where
  toHtmlRaw = toHtml

  toHtml (StationIdInput (Just selectedId)) = makeInputField "Station ID" "number" "station-id" (showt selectedId)
  toHtml (StationIdInput Nothing)           = makeInputField "Station ID" "number" "station-id" ""

  toHtml (TimeInput (Just selectedTime))    = makeInputField "Start Time" "datetime-local" "start-time" (formatTimeHtml selectedTime)
  toHtml (TimeInput Nothing)                = makeInputField "Start Time" "datetime-local" "start-time" ""

  toHtml (SubmitInput label)                = makeInputField label "submit" "" "Submit"


-- | A form to select various 'SelectionFormInput' parameters.
data SelectionForm where
  SelectionForm :: { selectionFormLegend :: T.Text
                   , selectionFormInputs :: [SelectionFormInput]
                   } -> SelectionForm

instance ToHtml SelectionForm where
  toHtmlRaw = toHtml
  toHtml (SelectionForm legend inputs) = do
    form_ [class_ "pure-form pure-form-stacked full-width", style_ "text-align: center"] $ fieldset_ $ do
          _ <- legend_ $ h3_ (toHtml legend)
          -- Grid layout for form
          div_ [class_ "pure-g full-width"] $
            mapM_ toHtml inputs


-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => T.Text -> T.Text -> T.Text -> T.Text -> HtmlT m ()
makeInputField label t id' val =
  label_ [for_ id', style_ "width: fit-content"] $
  (i_ . toHtml) label <>
  input_ [ type_ t
         , id_ (id' <> T.pack "-input")
         , name_ id'
         , class_ "pure-input-rounded"
         , value_ val
         , style_ "width: 95%"
         ]

formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale htmlTimeFormat
