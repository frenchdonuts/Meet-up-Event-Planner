-- Taken from: https://gist.github.com/TheSeamau5/25aede445f2942234588
--  and modified


module Components.Field exposing (..)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Validate exposing (..)
import String exposing (join)


-- The state of a field


type alias Field =
  { label : String
  , value : String
  , type' : String
  , autofocus : Bool
  , validator : Validator String String
  , errors : List String
  }



-- Test if a field is considered completed
-- In this case we consider a field complete if it is non-empty


fieldIsValid : Field -> Bool
fieldIsValid field =
  case field.validator field.value of
    -- "An empty error list means the subject was valid"
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
  , autofocus = False
  , validator = alwaysValid
  , errors = []
  }



-- Constructor for a required field


validatedField : String -> String -> Validator String String -> Field
validatedField label type' validator =
  { label = label
  , value = ""
  , type' = type'
  , autofocus = False
  , validator = validator
  , errors = []
  }


inputClass : Field -> String
inputClass field =
  let
    invalid =
      if fieldIsValid field then
        ""
      else
        "invalid"

    focus =
      if (field.autofocus) then
        "focus-field"
      else
        ""
  in
    focus ++ " " ++ invalid


containerClass : Field -> String
containerClass field =
  if field.type' == "datetime-local" then
    "col m6"
  else
    "input-field col m6"



-- The actions that can update a field


type Action
  = SetValue String
  | Validate
  | OnBlur
  | OnFocus



-- The update function for fields


update : Action -> Field -> Field
update action field =
  case action of
    SetValue value ->
      { field | value = value }

    Validate ->
      { field | errors = field.validator field.value }

    OnBlur ->
      update Validate field

    OnFocus ->
      field



-- The view function for fields


view : Field -> Html Action
view field =
  let
    -- Your CSS here
    containerStyle =
      []
  in
    H.div
      [ class "input-field col m6" ]
      [ H.input
          [ id (field.label ++ "-field")
          , class (inputClass field)
          , type' field.type'
          , value field.value
          , onInput SetValue
          , onBlur OnBlur
          , autocomplete True
          , autofocus field.autofocus
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

viewNoGrid : Field -> Html Action
viewNoGrid field =
  let
    -- Your CSS here
    containerStyle =
      []
  in
    H.div
      [ class "input-field" ]
      [ H.input
          [ id (field.label ++ "-field")
          , class (inputClass field)
          , type' field.type'
          , value field.value
          , onInput SetValue
          , onBlur OnBlur
          , onFocus OnFocus
          , autocomplete True
          , autofocus field.autofocus
          , placeholder ""
          ]
          []
      , label
          [ for (field.label ++ "-field")
          , attribute "data-error" (join "\n" field.errors)
          , class "active"
          , style [ ("margin-left", "-10.5px" )]
          ]
          [ text field.label ]
      ]

{-| Use this view when must display many validation error msgs.
-}
bigValidatedFieldView : Field -> List (Html Action)
bigValidatedFieldView field =
  let
    errors =
      List.map (\errStr -> li [] [ text errStr ]) field.errors

    divErrorStyle =
      [ ( "color", "#F44336" ) ]
  in
    [ H.div
        [ class "input-field col m6" ]
        [ H.input
            [ id (field.label ++ "-field")
            , class (inputClass field)
            , type' field.type'
            , value field.value
            , onInput SetValue
            , onBlur Validate
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
      -- Div where we display validation errors
    , H.ul
        [ class "col m6"
        , style divErrorStyle
        ]
        errors
    ]


-- Non-editable input


disabledView : Field -> Html Action
disabledView field =
  H.div
    [ class "input-field col m6" ]
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
