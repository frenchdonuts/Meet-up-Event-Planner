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
  CAF.Model



-- INIT


init =
  ( CAF.init, Effects.none )



-- VIEW
-- Examples of:
-- 1)  an externally defined component ('hello', takes 'model' as arg)
-- 2a) styling through CSS classes (external stylesheet)
-- 2b) styling using inline style attribute (two variants)


view address model =
  CAF.view address model



-- UPDATE


update action model =
  ( CAF.update action model, Effects.none )



-- CSS STYLES


styles =
  { wrapper =
      [ ( "padding-top", "10px" )
      , ( "padding-bottom", "20px" )
      , ( "text-align", "center" )
      ]
  }
