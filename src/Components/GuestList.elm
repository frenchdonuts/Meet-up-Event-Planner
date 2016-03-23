module Components.GuestList (..) where

import String exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Components.FormInput as F exposing (..)


-- MODEL


type alias Model =
  { guests : List ( ID, String )
  , addGuestInput : String
  , nextId : ID
  }


type alias ID =
  Int


init : Model
init =
  { guests = []
  , addGuestInput = ""
  , nextId = 0
  }



-- UPDATE


type Action
  = SetAddGuestInput String
  | AddGuest String
  | RemoveGuest Int


update : Action -> Model -> Model
update action model =
  case action of
    SetAddGuestInput guest ->
      { model | addGuestInput = guest }

    AddGuest guestName ->
      -- Make sure we don't add a guest with no name
      if not (String.isEmpty guestName) then
        { model
          | guests = model.guests ++ [ ( model.nextId, guestName ) ]
          , addGuestInput = ""
          , nextId = model.nextId + 1
        }
      else
        model

    RemoveGuest i ->
      let
        pred ( id, guestName ) =
          not <| i == id
      in
        { model
          | guests = List.filter pred model.guests
        }



-- VIEW
-- Alternative headers:
-- "Okay. Who's invited?"
-- "Oh no! No one's invited yet!", "At least you'll have some company." "They do say 3s a crowd..."


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    div
      [ class "row" ]
      [ list dispatcher model
      ]


list : Signal.Address Action -> Model -> Html
list dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    ul
      [ class "collection with-header" ]
      <| [ li
            [ class "collection-header", style [ ( "margin-top", "20px" ) ] ]
            [ div
                [ class "row" ]
                [ addGuestInput dispatcher model.addGuestInput
                , addBtn (contramapWith AddGuest) model.addGuestInput
                ]
              --, h4 [] [ text "Guests" ]
            ]
         ]
      ++ (List.map (\guest -> guestItem dispatcher guest) model.guests)


addGuestInput : Signal.Address Action -> String -> Html
addGuestInput dispatcher model =
  let
    contramapWithSetAddGuestInput =
      Signal.forwardTo dispatcher SetAddGuestInput

    emitOnEnterKey str keycode =
      -- Cuz 'return' apparently has a keycode of 13
      if keycode == 13 then
        AddGuest str
      else
        AddGuest ""
  in
    div
      [ class "input-field col s6 offset-s2" ]
      [ input
          [ id "add-guest-input"
          , type' "text"
          , on "input" targetValue (\str -> Signal.message dispatcher (SetAddGuestInput str))
          , onKeyPress dispatcher (emitOnEnterKey model)
          , value model
          , placeholder ""
          ]
          []
      , label [ for "add-guest-input", class "active" ] [ text "Guest Name" ]
      ]


addBtn : Signal.Address String -> String -> Html
addBtn dispatcher guest =
  button
    [ class "waves-effect waves-light btn col s2"
    , onClick dispatcher guest
    ]
    [ text "Add" ]


guestItem : Signal.Address Action -> ( ID, String ) -> Html
guestItem dispatcher ( id, name ) =
  li
    [ class "collection-item row" ]
    [ div
        []
        [ text name
        , span
            [ class "secondary-content" ]
            [ i
                [ class "material-icons"
                , onClick dispatcher (RemoveGuest id)
                ]
                [ text "close" ]
            ]
        ]
    ]
