module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Effects exposing (Effects)


-- official 'Elm Architecture' package
-- https://github.com/evancz/start-app

import StartApp


-- component import example

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
  { createEventForm : CEF.Model
  , guestList : GL.Model
  , createAccountForm : CAF.Model
  }



-- INIT


pureInit =
  { createEventForm = CEF.init
  , guestList = GL.init
  , createAccountForm = CAF.init
  }


init =
  ( pureInit, Effects.none )



-- UPDATE


type Action
  = CreateEventForm CEF.Action
  | GuestList GL.Action
  | CreateAccountForm CAF.Action


update action model =
  case action of
    CreateEventForm a ->
      ( { model | createEventForm = CEF.update a model.createEventForm }, Effects.none )

    GuestList a ->
      ( { model | guestList = GL.update a model.guestList }, Effects.none )

    CreateAccountForm a ->
      ( { model | createAccountForm = CAF.update a model.createAccountForm }, Effects.none )



-- VIEW
-- Swipable carousel of: CreateEventForm, GuestList, CreateAccountForm
--   DONE button when CreateAccountForm is visible


view dispatcher model =
  let
    forwardWith =
      Signal.forwardTo dispatcher
  in
    -- Html.form [ autocomplete True ]
    Html.form
      [ class "container", autocomplete True ]
      [ div
          [ class "row" ]
          [ h3 [] [ text "Hi! Tell me a little bit about your event." ] ]
      , CEF.view (forwardWith CreateEventForm) model.createEventForm
        --, GL.view (forwardWith GuestList) model.guestList
        --, CAF.view (forwardWith CreateAccountForm) model.createAccountForm
      ]


navbar =
  nav
    []
    [ div
        [ class "nav-wrapper" ]
        [ ul [ class "left" ] <| List.map (\str -> li [] [ text str ]) [ "Sign-Up", "Create Event", "Invite Guests", "Review" ]
        ]
    ]



{-
div
  [ class "mt-palette-accent", style styles.wrapper ]
  [ hello model
  , p [ style [ ( "color", "#FFF" ) ] ] [ text ("Elm Webpack Starter") ]
  , button [ class "mt-button-sm", onClick address Increment ] [ text "FTW!" ]
  , img [ src "img/elm.jpg", style [ ( "display", "block" ), ( "margin", "10px auto" ) ] ] []
  ]
-}
-- CSS STYLES


styles =
  { wrapper =
      [ ( "padding-top", "10px" )
      , ( "padding-bottom", "20px" )
      , ( "text-align", "center" )
      ]
  }
