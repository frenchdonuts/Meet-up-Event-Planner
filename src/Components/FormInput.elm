module Components.FormInput exposing (..)

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
  { inputType : InputType
  , inputAttrs : List (Attribute Action)
  , validator : Validator Err Value
  }


text_ : Model -> Validator Err Value -> Html Action
text_ =
  view_ "text"


password_ : Model -> Validator Err Value -> Html Action
password_ =
  view_ "password"


date_ : Model -> Html Action
date_ { label', value' } =
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
        , onInput SetValue
        --, on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
        , autocomplete True
        ]
        []
    ]


datetime_ : Model -> Html Action
datetime_ { label', value' } =
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
        , onInput SetValue
        --, on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
        , autocomplete True
        ]
        []
    ]


view_ inputType model validator =
  view
    { inputType = inputType, inputAttrs = [], validator = validator }
    model


view : Context -> Model -> Html Action
view context model =
  let
    { inputType, inputAttrs, validator } =
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
  in
    div
      [ class "input-field col s6" ]
      [ input
          ([ id (label' ++ "-field")
           , class inputClass
           , type' inputType
           , value value'
           , onInput SetValue
           --, on "input" targetValue (\str -> Signal.message dispatcher (SetValue str))
           , onBlur (Validate validator)
           --, validateWith validator
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


validate : Validator Err Value -> Attribute Action
validate validator =
  onBlur (Validate validator)
