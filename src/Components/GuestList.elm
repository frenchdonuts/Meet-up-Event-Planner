module Components.GuestList exposing (..)

import String exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as Json
import Validate exposing (..)
import Components.Field as F exposing (..)


-- MODEL


type alias Model =
  { guests : List ( ID, String )
  , addGuestInput : String
  , nextId : ID
  , maxCapacity : Field
  , errMsgs : List String
  }


type alias ID =
  Int


init : Model
init =
  let
    maxCapacityValidator =
      ifInvalid
        (\str ->
            case String.toInt str of
              Ok capacity -> if capacity > 0 then False else True
              Err _ -> True
        )
        "Max capacity must be a valid integer greater than 0!"
  in
    { guests = []
    , addGuestInput = ""
    , nextId = 0
    , maxCapacity =
        validatedField "How many people can come?" "number" maxCapacityValidator
    , errMsgs = []
    }

guestsLTmaxCapacityValidator : List ( ID, String ) -> String -> List String
guestsLTmaxCapacityValidator guests maxCapacityFieldValue =
  let
    errPredicate =
      let
        numOfGuests =
          Ok <| List.length guests
        maxCapacity =
          String.toInt maxCapacityFieldValue
      in
        case Result.map2 (>) numOfGuests maxCapacity of
          Ok isNumOfGuestsGTmaxCapacity ->
            isNumOfGuestsGTmaxCapacity
          Err _ ->
            False
  in
    if errPredicate then
      ["Too many guests! Hint: Either uninvite some people or increase the capacity."]
    else
      []

guestListEmptyValidator =
  ifInvalid List.isEmpty "It's going to be a lonely meetup! Please invite at least one person."

isComplete : Model -> Bool
isComplete model =
  fieldIsValid model.maxCapacity
    && not (List.isEmpty model.guests)
    && List.isEmpty model.errMsgs


-- UPDATE


type Action
  = SetAddGuestInput String
  | AddGuest String
  | RemoveGuest Int
  | UpdateMaxCapacityField F.Action
  | OnGuestNameInputBlur


update : Action -> Model -> Model
update action model =
  case action of
    SetAddGuestInput guest ->
      { model | addGuestInput = guest }

    AddGuest guestName ->
      let
        guests =
          -- Make sure we don't add a guest with no name
          if not (String.isEmpty guestName) then
            model.guests ++ [ ( model.nextId, guestName ) ]
          else
            model.guests
      in
        { model
          | guests = guests
          , addGuestInput = ""
          , nextId = model.nextId + 1
          , errMsgs = guestsLTmaxCapacityValidator guests model.maxCapacity.value
        }

    RemoveGuest i ->
      let
        pred ( id, guestName ) =
          not <| i == id
        guests =
          List.filter pred model.guests
        errMsgs =
          guestListEmptyValidator guests
          ++ guestsLTmaxCapacityValidator guests model.maxCapacity.value
      in
        { model
          | guests = guests
          , errMsgs = errMsgs
        }

    UpdateMaxCapacityField a ->
      let
        maxCapacity =
          F.update a model.maxCapacity
      in
        { model
        | maxCapacity = maxCapacity
        , errMsgs = guestsLTmaxCapacityValidator model.guests maxCapacity.value
        }

    OnGuestNameInputBlur ->
      { model
      | errMsgs = guestListEmptyValidator model.guests
      }



-- VIEW
view : Model -> Html Action
view model =
    div
      [ class "row" ]
      [ list model ]


list : Model -> Html Action
list model =
  let
    style' =
      []
  in
    ul
      [ class "collection with-header" ]
      <| [ li
            [ class "collection-header", style [ ( "margin-top", "20px" ) ] ]
            [ addGuestInput model.addGuestInput
            , addBtn model.addGuestInput
            , div
                [ class "col s8 m4 push-s2 push-m1" ]
                [ Html.App.map UpdateMaxCapacityField (F.viewNoGrid model.maxCapacity) ]
            , errView model.errMsgs
            , div [ class "row" ] []
            ]
         ]
      ++ (List.map (\guest -> guestItem guest) model.guests)

errView : List String -> Html Action
errView errMsgs =
  div
    [ class "col s11 m8 offset-s1 offset-m1"
    , style [ ("color", "#F44336") ]
    ]
    (List.map (\msg -> text msg) errMsgs)

addGuestInput : String -> Html Action
addGuestInput model =
  let
    options =
      { stopPropagation = True, preventDefault = True }
    keyDownDecoder =
      (Json.customDecoder
        keyCode
        (\code ->
            if code == 13 then
              Ok (AddGuest model)
            else
              Err "wrong key code"
        )
      )
  in
    div
      [ class "input-field col s8 m4 offset-s2 offset-m1" ]
      [ input
          [ id "add-guest-input"
          , class "focus-field"
          , type' "text"
          , onInput SetAddGuestInput
          , onBlur OnGuestNameInputBlur
            -- Allow User to add guest by pressing ENTER
          , onWithOptions "keypress" options keyDownDecoder
          , value model
          , placeholder ""
          ]
          []
      , label [ for "add-guest-input", class "active" ] [ text "Guest Name" ]
      ]


addBtn : String -> Html Action
addBtn guest =
  button
    [ type' "button"
    , class "waves-effect waves-light btn col s2 m1"
    , onClick (AddGuest guest)
    ]
    [ text "Add" ]


guestItem : ( ID, String ) -> Html Action
guestItem ( id, name ) =
  li
    [ class "collection-item row" ]
    [ div
        []
        [ text name
        , span
            [ class "secondary-content" ]
            [ i
                [ class "material-icons"
                , onClick (RemoveGuest id)
                ]
                [ text "close" ]
            ]
        ]
    ]
