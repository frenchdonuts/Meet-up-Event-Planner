module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Effects exposing (Effects)


-- official 'Elm Architecture' package
-- https://github.com/evancz/start-app

import StartApp


-- component import example

import Components.Hello exposing (hello)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)
import Components.CreateAccountForm as CAF exposing (..)


-- APP KICK OFF!


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html



-- HOT-SWAPPING


port swap : Signal.Signal Bool



-- MODEL


type alias Model =
  GL.Model



-- INIT


init =
  ( GL.init, Effects.none )



-- VIEW
-- Examples of:
-- 1)  an externally defined component ('hello', takes 'model' as arg)
-- 2a) styling through CSS classes (external stylesheet)
-- 2b) styling using inline style attribute (two variants)
-- Swipable carousel of: CreateEventForm, GuestList, CreateAccountForm
--   DONE button when CreateAccountForm is visible


view address model =
  GL.view address model



{-
div
  [ class "mt-palette-accent", style styles.wrapper ]
  [ hello model
  , p [ style [ ( "color", "#FFF" ) ] ] [ text ("Elm Webpack Starter") ]
  , button [ class "mt-button-sm", onClick address Increment ] [ text "FTW!" ]
  , img [ src "img/elm.jpg", style [ ( "display", "block" ), ( "margin", "10px auto" ) ] ] []
  ]
-}
-- UPDATE


update action model =
  ( GL.update action model, Effects.none )



-- CSS STYLES


styles =
  { wrapper =
      [ ( "padding-top", "10px" )
      , ( "padding-bottom", "20px" )
      , ( "text-align", "center" )
      ]
  }
