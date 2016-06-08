module Components.Thanks exposing (..)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)

type Msg = NoOp

view : Html Msg
view =
  div
    [ A.class "valign-wrapper" ]
    [ H.h2 [ A.class "valign" ] [ H.text "Have a nice day!" ] ]
