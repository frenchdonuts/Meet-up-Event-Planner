module Components.Summary (..) where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)


view : Signal.Address CEF.Action -> CEF.Model -> Signal.Address GL.Action -> GL.Model -> Html
view cefDispatcher cefModel glDispatcher glModel =
  div
    [ class "row" ]
    [ CEF.view cefDispatcher cefModel
    , div [ class "row" ] []
    , GL.view glDispatcher glModel
    ]
