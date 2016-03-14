module Components.FormInput (..) where

import Date
import Html exposing (..)
import Html.Attributes exposing (id, value, target, type', for)
import Html.Events exposing (on, targetValue)


-- MODEL


type alias Model =
  { label : String
  , value : String
  }


init =
  { label = ""
  , value = ""
  }


withLabel label =
  { label = label
  , value = ""
  }



-- UPDATE


type Action
  = SET_FIELD String


update : Action -> Model -> Model
update action model =
  case action of
    SET_FIELD value ->
      { model | value = value }



-- VIEW


type alias InputType =
  String


type alias Context =
  { dispatcher : Signal.Address Action
  , inputType : InputType
  }


text_ : Signal.Address Action -> Model -> Html
text_ =
  view_ "text"


password_ : Signal.Address Action -> Model -> Html
password_ =
  view_ "password"


date_ : Signal.Address Action -> Model -> Html
date_ =
  view_ "date"


view_ inputType dispatcher model =
  view
    { dispatcher = dispatcher, inputType = inputType }
    model


view : Context -> Model -> Html
view context model =
  let
    { dispatcher, inputType } =
      context

    label' =
      model.label

    value' =
      model.value
  in
    div
      []
      [ label [ for (label' ++ "-field") ] [ text label' ]
      , input
          [ id (label' ++ "-field")
          , type' inputType
          , value value'
          , on "input" targetValue <| \str -> Signal.message dispatcher (SET_FIELD str)
          ]
          []
      ]
