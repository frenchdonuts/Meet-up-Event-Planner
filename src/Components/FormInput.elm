module Components.FormInput (..) where

import Date
import String exposing (join)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Validate exposing (..)


-- MODEL


type alias Err =
  String


type alias Value =
  String


type alias Model =
  { label' : String
  , value' : String
  , validationResult : List String
  }


init : String -> String -> Model
init label value =
  { label' = label
  , value' = value
  , validationResult = []
  }


alwaysValid =
  ifInvalid (\_ -> False) ""



-- UPDATE


type Action
  = SetValue String
  | Reset
  | Validate (Validator Err Value)


update : Action -> Model -> Model
update action model =
  case action of
    SetValue value ->
      { model | value' = value }

    Reset ->
      { model | value' = "" }

    Validate validator ->
      { model | validationResult = validator model.value' }



-- VIEW


type alias InputType =
  String


type alias Context =
  { dispatcher : Signal.Address Action
  , inputType : InputType
  , inputAttrs : List Attribute
  , validator : Validator Err Value
  }


text_ : Signal.Address Action -> Model -> Validator Err Value -> Html
text_ =
  view_ "text"


password_ : Signal.Address Action -> Model -> Validator Err Value -> Html
password_ =
  view_ "password"


date_ : Signal.Address Action -> Model -> Html
date_ dispatcher { label', value' } =
  div
    [ class "col s6" ]
    [ label
        [ for (label' ++ "-field")
        ]
        [ text label' ]
    , input
        [ id (label' ++ "-field")
        , type' "date"
        , value value'
        , on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
        , autocomplete True
        ]
        []
    ]


datetime_ : Signal.Address Action -> Model -> Html
datetime_ dispatcher { label', value' } =
  div
    [ class "col s6" ]
    [ label
        [ for (label' ++ "-field")
        ]
        [ text label' ]
    , input
        [ id (label' ++ "-field")
        , type' "datetime-local"
        , value value'
        , on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
        , autocomplete True
        ]
        []
    ]


view_ inputType dispatcher model validator =
  view
    { dispatcher = dispatcher, inputType = inputType, inputAttrs = [], validator = validator }
    model


view : Context -> Model -> Html
view context model =
  let
    { dispatcher, inputType, inputAttrs, validator } =
      context

    -- Move label' to context^ ?
    { label', value', validationResult } =
      model

    inputClass =
      case validationResult of
        [] ->
          ""

        _ ->
          "invalid"

    validateWith =
      validate dispatcher
  in
    div
      [ class "input-field col s6" ]
      [ input
          ([ id (label' ++ "-field")
           , class inputClass
           , type' inputType
           , value value'
           , on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
           , validateWith validator
           , autocomplete True
           , placeholder ""
           ]
            ++ inputAttrs
          )
          []
      , label
          [ for (label' ++ "-field")
          , attribute "data-error" (join "," validationResult)
          ]
          [ text label' ]
      ]


validate : Signal.Address Action -> Validator Err Value -> Attribute
validate dispatcher validator =
  onBlur dispatcher (Validate validator)
