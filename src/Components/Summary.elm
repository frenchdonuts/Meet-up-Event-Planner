module Components.Summary exposing (..)

import Html as H exposing (..)
import Html.App exposing (map)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)

type Action
  = UpdateCreateEventForm CEF.Action
  | UpdateGuestList GL.Action


view : CEF.Model -> GL.Model -> Html Action
view cefModel glModel =
  div
    [ class "row" ]
    [ map UpdateCreateEventForm (CEF.view cefModel)
    --CEF.view cefDispatcher cefModel
    , div [ class "row" ] []
    , map UpdateGuestList (GL.view glModel)
    --, GL.view glDispatcher glModel
    ]
