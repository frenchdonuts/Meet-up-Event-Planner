-- Taken from: https://gist.github.com/TheSeamau5/25aede445f2942234588
module Components.Field (..) where

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Signal exposing (Address)
import Validate exposing (..)


-- The state of a field
type alias Field =
  { label : String
  , value : String
  , isRequired : Bool
  }

-- Test if a field is considered completed
-- In this case we consider a field complete if it is non-empty
fieldIsCompleted : Field -> Bool
fieldIsCompleted field =
  if field.isRequired
  then
    String.length field.value > 0
  else
    True


-- Constructor for an optional field
optionalField : String -> Field
optionalField label =
  { label = label
  , value = ""
  , isRequired = False
  }

-- Constructor for a required field
requiredField : String -> Field
requiredField label =
  { label = label
  , value = ""
  , isRequired = True
  }


labelColor : Field -> String
labelColor field =
  if not field.isRequired
  then "black"
  else
    if fieldIsCompleted field
    then
      "green"
    else
      "red"

-- The actions that can update a field
type FieldAction
  = SetValue String


-- The update function for fields
updateField : FieldAction -> Field -> Field
updateField action field =
  case action of
    SetValue value ->
      { field | value <- value }


-- The view function for fields
viewField : Address FieldAction -> Field -> Html
viewField address field =
  let
      -- Your CSS here
      containerStyle =
          []

      labelStyle =
          [ "color" => labelColor field ]
  in
      Html.div
          [ Html.Attributes.style containerStyle ]
          [ Html.span
                [ Html.Attributes.style labelStyle ]
                [ Html.text field.label ]
          , Html.input
                [ onInput address SetValue
                , Html.Attributes.value field.value
                ]
                []
          ]
