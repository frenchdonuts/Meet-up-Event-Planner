module Components.LocationSearcher exposing (..)

import Task
import Http
import Json.Decode as J
import Components.Field as F
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import List as L
import Validate exposing (..)
import Process
import Time
import Geolocation exposing (..)
import Debug
import String
import Array
--  Ugh...I can't use elm-autocomplete b/c their input doesn't have a label...
--import Autocomplete
--import Autocomplete.Styling as Styling


-- MODEL
type alias Model =
  { searchField : F.Field
  , sleepCount : Int
  , venueItems : List VenueItem
  , activeVenueItem : Int
  , location : Maybe Location
  , menuVisible : Bool
  , userInput : String
  }

type alias VenueItem =
  { index : Int
  , isActive : Bool
  , venue : Venue
  }
venueItem : Int -> Venue -> VenueItem
venueItem index venue =
  { index = index
  , isActive = False
  , venue = venue
  }

type alias Venue =
  { name : String
  , address : Maybe String
  }

init : (Model, Cmd Msg)
init =
  ({ searchField = F.validatedField "Location: " "text" (ifBlank "Where is the event going to be held?")
   , sleepCount = 0
   , venueItems = []
   , activeVenueItem = -1
   , location = Nothing
   , menuVisible = False
   , userInput = ""
   }
   -- Fetch User location using Geolocation
   , Task.perform FetchLocationFail FetchLocationSucceed now
   )


isComplete : Model -> Bool
isComplete model =
  F.fieldIsValid model.searchField

-- UPDATE
type Msg
  = UpdateSearchField F.Action
  | FetchVenues
  | FetchVenuesSucceed (List Venue)
  | FetchVenuesFail Http.Error
  | Timeout Int
  | FetchLocationSucceed Location
  | FetchLocationFail Error
  | HideMenu
  | ShowMenu
  | OnArrowUp
  | OnArrowDown
  | OnVenueItemHover Int
  | SelectVenue

timeToSettle =
  300 * Time.millisecond

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSearchField fieldAction ->
      case fieldAction of
        F.SetValue _ ->
          let
            newCount =
              model.sleepCount + 1
            searchField =
              F.update fieldAction model.searchField
            menuVisible =
              not (String.isEmpty searchField.value)
          in
            ( { model
              | searchField = searchField
              , userInput = searchField.value
              , sleepCount = newCount
              , menuVisible = menuVisible
              }
            -- Debounce
            , Process.sleep timeToSettle |> Task.perform never (always (Timeout newCount))
            )

        F.OnBlur ->
          ( { model | menuVisible = False }, Cmd.none )

        F.OnFocus ->
          ( { model | menuVisible = True }, Cmd.none )

        _ -> ( { model | searchField = F.update fieldAction model.searchField }, Cmd.none )

    Timeout count ->
      if count == model.sleepCount then
        update FetchVenues { model | sleepCount = 0 }
      else
        ( model, Cmd.none )

    FetchVenues ->
      Debug.log "Fetching venues" ( model, searchHostLocation model.searchField.value model.location )

    FetchVenuesSucceed venues ->
      let
        menuVisible =
          not (L.isEmpty venues)
      in
        ( { model
          | venueItems = List.indexedMap (\i v -> venueItem i v) venues
          , menuVisible = menuVisible
          , activeVenueItem = -1
          }
        , Cmd.none )

    FetchVenuesFail error ->
      ( model, Cmd.none )

    FetchLocationSucceed location ->
      ( { model | location = Just location }, Cmd.none )

    FetchLocationFail error ->
      ( { model | location = Nothing }, Cmd.none )

    HideMenu ->
      Debug.log "Hiding menu..." ( { model | menuVisible = False }, Cmd.none )
    ShowMenu ->
      let
        menuVisible =
          L.isEmpty model.venueItems
      in
        ( {model | menuVisible = menuVisible }, Cmd.none)

    OnArrowUp ->
      let
        activeVenueItem' =
          Basics.max (model.activeVenueItem - 1) -1
        model' =
          setVenueItemActive activeVenueItem' model
      in
        ( { model'
          | menuVisible = True
          }
        , Cmd.none )

    OnArrowDown ->
      let
        activeVenueItem' =
          if (model.activeVenueItem > (L.length model.venueItems) - 1) then
            -1
          else
            model.activeVenueItem + 1
        model' =
          setVenueItemActive activeVenueItem' model
      in
        ( { model'
          | menuVisible = True
          }
        , Cmd.none )

    OnVenueItemHover i ->
      let
        activeVenueItem' = i
      in
        ( setVenueItemActive activeVenueItem' model, Cmd.none )

    SelectVenue ->
        ( { model | menuVisible = False }, Cmd.none )

setVenueItemActive : Int -> Model -> Model
setVenueItemActive activeVenueItem' model =
  let
    venueItems =
      model.venueItems
    activeVenueName =
      let
        extractValue maybe =
          case maybe of
            Just venue -> venue.name
            Nothing -> ""
      in
        venueItems
        |> L.filter (\venueItem -> venueItem.index == activeVenueItem')
        |> L.map .venue
        |> Array.fromList
        |> Array.get 0
        |> extractValue
    inputValue =
      if activeVenueItem' > -1 then
        activeVenueName
      else
        model.userInput
    f venueItem =
      if not (venueItem.index == activeVenueItem') then
        { venueItem | isActive = False }
      else
          { venueItem | isActive = True }
  in
    { model
    | activeVenueItem = activeVenueItem'
    , venueItems = List.map f venueItems
    , searchField = F.update (F.SetValue inputValue) model.searchField
    }

-- Commands


searchHostLocation : String -> Maybe Location -> Cmd Msg
searchHostLocation userInput location =
  let
    noLocationResult =
      [ { name = "I need geolocation to search for possible event locations!", address = Nothing } ]
  in
    if userInput == "" then
      Task.perform never FetchVenuesSucceed (Task.succeed [])
    else
      case location of
        Just loc ->
          Task.perform FetchVenuesFail FetchVenuesSucceed (httpGetTask userInput loc)
        Nothing ->
          Task.perform never FetchVenuesSucceed (Task.succeed [])

httpGetTask : String -> Location -> Task.Task Http.Error (List Venue)
httpGetTask searchQuery loc =
  let
    rootUrl =
      "https://api.foursquare.com/v2/venues/search"
    latitude =
      toString loc.latitude
    longitude =
      toString loc.longitude
    url = Http.url
            rootUrl
            [ ("client_id", "FY2GEHLVGVML0HJN34QEOUIIS24T2OCPFF2KXMX10PRAX0BP")
            , ("client_secret", "1DEZMNNOY4XHT4LA1DUDAYZM1USOJD04QR24L0B1YYCFLYWN")
            , ("ll", latitude ++ "," ++ longitude)
            , ("v", "20160606")
            , ("m", "foursquare")
            , ("limit", "9")
            , ("query", searchQuery)
            ]
  in
    Http.get venuesDecoder url

venuesDecoder : J.Decoder (List Venue)
venuesDecoder =
  J.at ["response", "venues"] (J.list venueDecoder)

venueDecoder : J.Decoder Venue
venueDecoder =
  J.object2 Venue venueNameDecoder venueAddressDecoder

venueNameDecoder : J.Decoder String
venueNameDecoder =
  J.at ["name"] J.string

venueAddressDecoder : J.Decoder (Maybe String)
venueAddressDecoder =
  J.at ["location", "address"] J.string |> J.maybe


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW
view : Model -> Html Msg
view model =
  let
    menuStyles =
      []-- ( "z-index", "10000" ) ]

    options =
      { preventDefault = True, stopPropagation = False }

    keyDownDecoder =
      (J.customDecoder
        keyCode
        (\code ->
            if code == 38 then
              Ok OnArrowUp
            else if code == 40 then
              Ok OnArrowDown
            else if code == 13 then
              Ok SelectVenue
            else
              Err "not handling that key"
        )
      )
  in
    div
      [ class "col s6"
      , style menuStyles
      -- Why won't these work!?!
      , onFocus ShowMenu
      , onBlur HideMenu
      , onWithOptions "keydown" options keyDownDecoder
      ]
      [ viewInput model.searchField --map UpdateSearchField (F.view' model.searchField [])
      , searchResultsView model.menuVisible model.venueItems model.searchField.value
      ]

viewInput : F.Field -> Html Msg
viewInput searchField =
    map UpdateSearchField (F.viewNoGrid searchField)

searchResultsView : Bool -> List VenueItem -> String -> Html Msg
searchResultsView menuVisible venueItems searchField =
  let
    listStyles =
      [ ( "margin-top", "-14px" )
      , ( "position", "absolute" )
      --, ( "opacity", "1" )
      , ( "z-index", "10000" )
      ]
  in
    ul
      [ class "collection"
      , hidden ( not menuVisible )
      , style listStyles
      ]
      ( L.map (\venueItem -> venueItemView venueItem) venueItems )

venueItemView : VenueItem -> Html Msg
venueItemView venueItem =
  let
    class' =
      if venueItem.isActive then
        "collection-item active"
      else
        "collection-item"
    venue =
      venueItem.venue
    address =
      case venue.address of
        Just a -> a
        Nothing -> ""
    viewStyle =
      []-- ( "background-color", "white" ) ]
  in
    li
      [ class class'
      , style viewStyle
      , onMouseOver (OnVenueItemHover venueItem.index)
      ]
      [ div
          []
          [ text venue.name
          , br [] []
          , text address
          ]
      ]

never : Never -> a
never n =
  never n
