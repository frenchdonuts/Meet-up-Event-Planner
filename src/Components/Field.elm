-- Taken from: https://gist.github.com/TheSeamau5/25aede445f2942234588
--  and modified


module Components.Field (..) where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Signal exposing (Address)
import Validate exposing (..)
import String exposing (join)


-- The state of a field


type alias Field =
  { label : String
  , value : String
  , type' : String
  , validator : Validator String String
  , errors : List String
  }



-- Test if a field is considered completed
-- In this case we consider a field complete if it is non-empty


fieldIsValid : Field -> Bool
fieldIsValid field =
  case field.validator field.value of
    [] ->
      True

    _ ->
      False


alwaysValid : Validator String String
alwaysValid =
  ifInvalid (\_ -> False) ""



-- Constructor for an optional field


optionalField : String -> String -> Field
optionalField label type' =
  { label = label
  , value = ""
  , type' = type'
  , validator = alwaysValid
  , errors = []
  }



-- Constructor for a required field


validatedField : String -> String -> Validator String String -> Field
validatedField label type' validator =
  { label = label
  , value = ""
  , type' = type'
  , validator = validator
  , errors = []
  }


inputClass : Field -> String
inputClass field =
  if fieldIsValid field then
    ""
  else
    "invalid"


containerClass : Field -> String
containerClass field =
  if field.type' == "datetime-local" then
    "col s6"
  else
    "input-field col s6"



-- The actions that can update a field


type Action
  = SetValue String
  | Validate



-- The update function for fields


update : Action -> Field -> Field
update action field =
  case action of
    SetValue value ->
      { field | value = value }

    Validate ->
      { field | errors = field.validator field.value }



-- The view function for fields


view : Address Action -> Field -> Html
view address field =
  let
    -- Your CSS here
    containerStyle =
      []
  in
    H.div
      [ class "input-field col s6" ]
      --(containerClass field) ]
      [ H.input
          [ id (field.label ++ "-field")
          , class (inputClass field)
          , type' field.type'
          , value field.value
          , onInput address SetValue
          , onBlur address Validate
          , autocomplete True
          , placeholder ""
          ]
          []
      , label
          [ for (field.label ++ "-field")
          , attribute "data-error" (join "\n" field.errors)
          , class "active"
          ]
          [ text field.label ]
      ]



-- Use this view when you have too many validation error msgs


bigValidatedFieldView : Address Action -> Field -> List Html
bigValidatedFieldView address field =
  let
    errors =
      List.map (\errStr -> li [] [ text errStr ]) field.errors

    --      join "\n" field.errors
    divErrorStyle =
      [ ( "color", "#F44336" ) ]
  in
    [ H.div
        [ class "input-field col s6" ]
        [ H.input
            [ id (field.label ++ "-field")
            , class (inputClass field)
            , type' field.type'
            , value field.value
            , onInput address SetValue
            , onBlur address Validate
            , autocomplete True
            , placeholder ""
            ]
            []
        , label
            [ for (field.label ++ "-field")
            , class "active"
            ]
            [ text field.label ]
        ]
      -- Display validation errors in this div
    , H.ul
        [ class "col s6"
        , style divErrorStyle
        ]
        errors
    ]


onInput : Address a -> (String -> a) -> Attribute
onInput address constructor =
  E.on "input" E.targetValue (constructor >> Signal.message address)



-- Non-editable input


disabledView : Field -> Html
disabledView field =
  H.div
    [ class "input-field col s6" ]
    --(containerClass field) ]
    [ H.input
        [ id (field.label ++ "-field")
        , disabled True
        , class (inputClass field)
        , type' field.type'
        , value field.value
        , placeholder ""
        ]
        []
    , label
        [ for (field.label ++ "-field")
        , class "active"
        ]
        [ text field.label ]
    ]
