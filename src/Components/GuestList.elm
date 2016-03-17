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
  }


type alias ID =
  Int


init : Model
init =
  { guests = []
  , addGuestInput = ""
  }



-- UPDATE


type Action
  = SetAddGuestInput String
  | AddGuest String
  | RemoveGuest Int


update action model =
  case action of
    SetAddGuestInput guest ->
      { model | addGuestInput = guest }

    AddGuest guest ->
      -- Make sure we don't add a guest with no name
      if not (String.isEmpty guest) then
        { model
          | guests = model.guests ++ [ guest ]
          , addGuestInput = ""
        }
      else
        model

    RemoveGuest i ->
      { model
        | guests = List.filter (\guest -> not i == guest.id) model.guests
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
      []
      [ h1 [] [ text "Who are the special people?" ]
      , addGuestInput dispatcher model.addGuestInput
      , addBtn (contramapWith AddGuest) model.addGuestInput
      , list dispatcher model.guests
      ]


addGuestInput dispatcher model =
  let
    contramapWithSetAddGuestInput =
      Signal.forwardTo dispatcher SetAddGuestInput

    emitOnEnterKey str keycode =
      if keycode == 13 then
        AddGuest str
      else
        AddGuest ""
  in
    div
      [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
      [ label
          [ class "mdl-textfield__label"
          , for "add-guest-input"
          ]
          [ text "Guest Name" ]
      , input
          [ id "add-guest-input"
          , class "mdl-textfield__input"
          , on "input" targetValue (\str -> Signal.message dispatcher (SetAddGuestInput str))
          , onKeyPress dispatcher (emitOnEnterKey model)
          , value model
          ]
          []
      ]


list : Signal.Address Action -> List String -> Html
list dispatcher guests =
  div [ class "mdl-list" ] (List.map (\guest -> guestItem dispatcher guest) guests)


guestItem : Signal.Address Action -> String -> Html
guestItem dispatcher guest =
  div
    [ class "mdl-list__item" ]
    [ span [ class "mdl-list__item-primary-content" ] [ text guest ]
    , i
        [ class "material-icons"
        , onClick dispatcher Delete
        ]
        [ text "star" ]
    ]


addBtn : Signal.Address String -> String -> Html
addBtn dispatcher guest =
  button
    [ class "mdl-button mdl-js-button mdl-button--raised mdl-button--colored"
    , onClick dispatcher guest
    ]
    [ text "Add" ]
