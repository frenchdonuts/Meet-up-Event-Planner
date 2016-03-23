module Components.Thanks (..) where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)


view : Html
view =
  div
    [ class "valign-wrapper" ]
    [ h2 [ class "valign" ] [ text "Have a nice day!" ] ]
