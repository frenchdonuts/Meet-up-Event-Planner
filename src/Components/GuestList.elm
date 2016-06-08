module Components.GuestList exposing (..)

import String exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as Json
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


view : Model -> Html Action
view model =
    div
      [ class "row" ]
      [ list model ]


list : Model -> Html Action
list model =
    ul
      [ class "collection with-header" ]
      <| [ li
            [ class "collection-header", style [ ( "margin-top", "20px" ) ] ]
            [ div
                [ class "row" ]
                [ addGuestInput model.addGuestInput
                , addBtn model.addGuestInput
                ]
              --, h4 [] [ text "Guests" ]
            ]
         ]
      ++ (List.map (\guest -> guestItem guest) model.guests)


addGuestInput : String -> Html Action
addGuestInput model =
  let
    is13 code =
      if code == 13 then
        Ok (AddGuest model)
      else
        Err "wrong key code"
  in
    div
      [ class "input-field col s6 offset-s2" ]
      [ input
          [ id "add-guest-input"
          , class "focus-field"
          , type' "text"
          , onInput SetAddGuestInput
            -- Allow User to add guest by pressing ENTER
          , onWithOptions
              "keypress"
              -- preventDefault since we are inside a <form>
              { stopPropagation = True, preventDefault = True }
              (Json.customDecoder keyCode is13)
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
    , class "waves-effect waves-light btn col s2"
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
