{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}

-- |

module Server.Page.SelectionForm
     ( SelectionForm (..)
     , SelectionFormInput (..)
     , StationListFilter (..)
     , formatTimeHtml
     , makeInputField
     ) where

import           Control.Applicative  ( (<|>) )
import           Control.Monad        ( forM_ )

import           Data.Attoparsec.Text
import           Data.Functor         ( ($>) )
import           Data.String          ( IsString )
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Extras

import           Lucid

import           Servant              ( FromHttpApiData (..), ToHttpApiData (..) )

import           Server.Page.Utils    ( mkData_ )

import           TextShow             ( showt )


inputCheckedIf_ :: Applicative m => Bool -> [Attribute] -> HtmlT m ()
inputCheckedIf_ cond attrs =
  if cond
  then input_ (checked_ : attrs)
  else input_ attrs


-- | Values used to select station list filter parameters.
data StationListFilter where
  AllStations      :: StationListFilter
  RegularStations  :: StationListFilter
  ChargingStations :: StationListFilter
  deriving stock (Eq, Show)

instance FromHttpApiData StationListFilter where
  parseUrlPiece :: T.Text -> Either T.Text StationListFilter
  parseUrlPiece p = case parseOnly (asciiCI "all"      $> AllStations     <|>
                                    asciiCI "regular"  $> RegularStations <|>
                                    asciiCI "charging" $> ChargingStations
                                   ) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

-- | Lucid ToHtml instance.
instance ToHtml StationListFilter where
  toHtmlRaw = toHtml

  toHtml stationFilter = do
    labelFor "station-type-radio-all" $
      inputCheckedIfSelection_ AllStations [id_ "station-type-radio-all", type_ "radio", name_ "station-type-radio", value_ "all", mkData_ "station-type" "all"] <> span_ "All"

    labelFor "station-type-radio-regular" $
      inputCheckedIfSelection_ RegularStations [id_ "station-type-radio-regular", type_ "radio", name_ "station-type-radio", value_ "regular", mkData_ "station-type" "regular"] <> span_ "Regular"

    labelFor "station-type-radio-charging" $
      inputCheckedIfSelection_ ChargingStations [id_ "station-type-radio-charging", type_ "radio", name_ "station-type-radio", value_ "charging", mkData_ "station-type" "charging"] <> span_ "Charging"
    where
      labelFor forElement = label_ [for_ forElement, class_ "pure-radio"]
      inputCheckedIfSelection_ :: Applicative m => StationListFilter -> [Attribute] -> HtmlT m ()
      inputCheckedIfSelection_ selection =
        inputCheckedIf_ (stationFilter == selection)

-- | Servant 'ToHttpData' instance.
instance ToHttpApiData StationListFilter where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


-- | Various inputs used in 'SelectionForm'.
data SelectionFormInput where
  StationTypeInput :: StationListFilter -> SelectionFormInput
  StationIdInput   :: Maybe Int         -> SelectionFormInput
  SearchInput      :: T.Text -> T.Text  -> SelectionFormInput
  TimeInput        :: Maybe LocalTime   -> SelectionFormInput
  SubmitInput      :: T.Text            -> SelectionFormInput

instance ToHtml SelectionFormInput where
  toHtmlRaw = toHtml

  toHtml (StationIdInput (Just selectedId)) = makeInputField "Station ID" "number" "station-id" (showt selectedId)
  toHtml (StationIdInput Nothing)           = makeInputField "Station ID" "number" "station-id" ""

  toHtml (SearchInput inputId placeholder)  =
    div_ [class_ " station-search-input-outer full-width"] $
        label_ [for_ inputId] "Filter" <> input_ [id_ inputId, class_ "pure-input-rounded station-search-input", style_ "margin: 0 auto;", type_ "search", placeholder_ placeholder]

  toHtml (StationTypeInput stationType)     = toHtml stationType

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
            forM_ inputs $
                div_ [class_ inputWrapperClass] . toHtml
    where
      inputWrapperClass = T.pack ("pure-u-1-" <> (show . length) inputs)


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
