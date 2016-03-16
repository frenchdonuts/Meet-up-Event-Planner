module Components.GuestList (..) where

import List exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Components.FormInput as F exposing (..)


-- MODEL


type alias Model =
  { guests : List String
  , addGuestInput : F.Model
  }


init : Model
init =
  { guests = []
  , addGuestInput = F.withLabel "Add Guest: "
  }



-- UPDATE


type Action
  = SetAddGuestInput F.Action
  | AddGuest String


update action model =
  case action of
    SetAddGuestInput a ->
      { model | addGuestInput = F.update a model.addGuestInput }

    AddGuest guest ->
      { model
        | guests = model.guests ++ [ guest ]
        , addGuestInput = F.update F.Reset model.addGuestInput
      }



-- VIEW


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    div
      []
      [ h1 [] [ text "Okay. Who's invited?" ]
      , F.text_ (contramapWith SetAddGuestInput) model.addGuestInput
      , addBtn dispatcher (AddGuest model.addGuestInput.value)
      , list model.guests
      ]


list : List String -> Html
list guests =
  ul [ class "mdl-list" ] (map (\guest -> guestItem guest) guests)


guestItem : String -> Html
guestItem guest =
  li
    [ class "mdl-list__item" ]
    [ span [ class "mdl-list__item-primary-content" ] [ text guest ] ]


addBtn : Signal.Address Action -> Action -> Html
addBtn dispatcher action =
  button
    [ class "mdl-button mdl-js-button mdl-button--raised mdl-button--colored"
    , onClick dispatcher action
    ]
    [ text "Add" ]
