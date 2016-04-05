module Components.Thanks (..) where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)


view : Html
view =
  div
    [ A.class "valign-wrapper" ]
    [ H.h2 [ A.class "valign" ] [ H.text "Have a nice day!" ] ]
