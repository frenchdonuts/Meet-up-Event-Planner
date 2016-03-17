module Components.FormInput (..) where

import Date
import Html exposing (..)
import Html.Attributes exposing (class, id, value, target, type', for, autocomplete)
import Html.Events exposing (on, targetValue)


-- MODEL


type alias Model =
  { label : String
  , value : String
  }


init : Model
init =
  { label = ""
  , value = ""
  }


withLabel : String -> Model
withLabel label =
  { label = label
  , value = ""
  }



-- UPDATE


type Action
  = SET_FIELD String
  | Reset


update : Action -> Model -> Model
update action model =
  case action of
    SET_FIELD value ->
      { model | value = value }

    Reset ->
      { model | value = "" }



-- VIEW


type alias InputType =
  String


type alias Context =
  { dispatcher : Signal.Address Action
  , inputType : InputType
  , inputAttrs : List Attribute
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


datetime_ : Signal.Address Action -> Model -> Html
datetime_ =
  view_ "datetime-local"


view_ inputType dispatcher model =
  view
    { dispatcher = dispatcher, inputType = inputType, inputAttrs = [] }
    model


view : Context -> Model -> Html
view context model =
  let
    { dispatcher, inputType, inputAttrs } =
      context

    label' =
      model.label

    value' =
      model.value
  in
    div
      [ class "mdl-textfield mdl-js-textfield mdl-textfield--floating-label" ]
      [ label
          [ class "mdl-textfield__label"
          , for (label' ++ "-field")
          ]
          [ text label' ]
      , input
          ([ id (label' ++ "-field")
           , class "mdl-textfield__input"
           , type' inputType
           , value value'
           , on "input" targetValue (\str -> Signal.message dispatcher (SET_FIELD str))
           , autocomplete True
           ]
            ++ inputAttrs
          )
          []
      ]
