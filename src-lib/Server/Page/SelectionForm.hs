{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}

-- |

module Server.Page.SelectionForm
     ( OrderByDirection (..)
     , OrderByOption (..)
     , SelectionForm (..)
     , SelectionFormInput (..)
     , StationListFilter (..)
     , formatTimeHtml
     , makeInputField
     ) where

import           Control.Applicative  ( (<|>) )
import           Control.Monad        ( forM_ )

import           Data.Attoparsec.Text
import           Data.Functor         ( ($>) )
import qualified Data.Text            as T
import           Data.Time
import           Data.Time.Extras

import           Lucid

import           Servant              ( FromHttpApiData (..), ToHttpApiData (..) )

import           Server.Page.Utils    ( mkData_ )

import           TextShow             ( showt )


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
    mkRadio stationFilter AllStations      "station-type-radio-all"      "all"      "All"
    mkRadio stationFilter RegularStations  "station-type-radio-regular"  "regular"  "Regular"
    mkRadio stationFilter ChargingStations "station-type-radio-charging" "charging" "Charging"

inputCheckedIf_ :: Applicative m => Bool -> [Attribute] -> HtmlT m ()
inputCheckedIf_ cond attrs
  | cond      = input_ (checked_ : attrs)
  | otherwise = input_ attrs

mkRadio :: Monad m => StationListFilter -> StationListFilter -> T.Text -> T.Text -> HtmlT m () -> HtmlT m ()
mkRadio stationFilter checkedIf rId rValue rContent =
  labelFor rId $ do
    inputCheckedIf_ (stationFilter == checkedIf) attrs
    nbsp_ <> rContent
  where attrs = [id_ rId, type_ "radio", name_ "station-type-radio", value_ rValue, mkData_ "station-type" rValue]

nbsp_ :: Monad m => HtmlT m ()
nbsp_ = toHtmlRaw ("&nbsp" :: T.Text)

labelFor :: Term [Attribute] result => T.Text -> result
labelFor forElement = label_ [for_ forElement, class_ "pure-radio"]

-- | Servant 'ToHttpData' instance.
instance ToHttpApiData StationListFilter where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


-- | Options for the direction of the ordering of the station list.
data OrderByDirection where
  OrderByAsc  :: OrderByDirection
  OrderByDesc :: OrderByDirection
  deriving stock (Eq, Show)

instance FromHttpApiData OrderByDirection where
  parseUrlPiece :: T.Text -> Either T.Text OrderByDirection
  parseUrlPiece p = case parseOnly (asciiCI "asc"  $> OrderByAsc <|>
                                    asciiCI "desc" $> OrderByDesc) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

instance ToHttpApiData OrderByDirection where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


-- | Options for ordering the station list.
data OrderByOption where
  OrderByStationId           :: OrderByOption
  OrderByStationName         :: OrderByOption
  OrderByStationType         :: OrderByOption
  OrderByStationCapacity     :: OrderByOption
  OrderByMechanicalAvailable :: OrderByOption
  OrderByEfitAvailable       :: OrderByOption
  OrderByEfitG5Available     :: OrderByOption
  OrderByBikesDisabled       :: OrderByOption
  OrderByTimeFull            :: OrderByOption
  OrderByTimeEmpty           :: OrderByOption
  deriving stock (Eq, Show)

instance FromHttpApiData OrderByOption where
  parseUrlPiece :: T.Text -> Either T.Text OrderByOption
  parseUrlPiece p = case parseOnly (asciiCI "station-id"           $> OrderByStationId           <|>
                                    asciiCI "station-name"         $> OrderByStationName         <|>
                                    asciiCI "station-type"         $> OrderByStationType         <|>
                                    asciiCI "station-capacity"     $> OrderByStationCapacity     <|>
                                    asciiCI "mechanical-available" $> OrderByMechanicalAvailable <|>
                                    asciiCI "efit-available"       $> OrderByEfitAvailable       <|>
                                    asciiCI "efit-g5-available"    $> OrderByEfitG5Available     <|>
                                    asciiCI "bikes-disabled"       $> OrderByBikesDisabled       <|>
                                    asciiCI "time-full"            $> OrderByTimeFull            <|>
                                    asciiCI "time-empty"           $> OrderByTimeEmpty
                                   ) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

instance ToHttpApiData OrderByOption where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


-- | Various inputs used in 'SelectionForm'.
data SelectionFormInput where
  OrderByOptionInput    :: OrderByOption     -> SelectionFormInput
  OrderByDirectionInput :: OrderByDirection  -> SelectionFormInput
  StationTypeInput      :: StationListFilter -> SelectionFormInput
  StationIdInput        :: Maybe Int         -> SelectionFormInput
  SearchInput           :: T.Text -> T.Text  -> SelectionFormInput
  TimeInput             :: Maybe LocalTime   -> SelectionFormInput
  SubmitInput           :: T.Text            -> SelectionFormInput

instance ToHtml SelectionFormInput where
  toHtmlRaw = toHtml

  toHtml (OrderByOptionInput opt)           = toHtml ((T.pack . show) opt)
  toHtml (OrderByDirectionInput dir)        = toHtml ((T.pack . show) dir)

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
