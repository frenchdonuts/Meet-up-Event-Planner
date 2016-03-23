module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Effects exposing (Effects)


-- official 'Elm Architecture' package
-- https://github.com/evancz/start-app

import StartApp
import Components.SelectionList exposing (..)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)
import Components.CreateAccountForm as CAF exposing (..)
import Components.Summary as S exposing (..)


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


type Page
  = CreateAccountForm
  | CreateEventForm
  | GuestList
  | Summary


type alias Model =
  { createEventForm : CEF.Model
  , guestList : GL.Model
  , createAccountForm : CAF.Model
  , pages : SelectionList Page
  }



-- INIT


pureInit =
  { createEventForm = CEF.init
  , guestList = GL.init
  , createAccountForm = CAF.init
  , pages = newSelectionList CreateAccountForm [ CreateEventForm, GuestList, Summary ]
  }


init =
  ( pureInit, Effects.none )



-- UPDATE


type Action
  = UpdateCreateEventForm CEF.Action
  | UpdateGuestList GL.Action
  | UpdateCreateAccountForm CAF.Action
  | NextPage
  | PrevPage


update action model =
  case action of
    UpdateCreateEventForm a ->
      ( { model | createEventForm = CEF.update a model.createEventForm }, Effects.none )

    UpdateGuestList a ->
      ( { model | guestList = GL.update a model.guestList }, Effects.none )

    UpdateCreateAccountForm a ->
      ( { model | createAccountForm = CAF.update a model.createAccountForm }, Effects.none )

    NextPage ->
      if curPageIsValid model.pages.current model then
        ( { model | pages = forward model.pages }, Effects.none )
      else
        ( model, Effects.none )

    PrevPage ->
      ( { model | pages = back model.pages }, Effects.none )


curPageIsValid : Page -> Model -> Bool
curPageIsValid page model =
  case page of
    CreateAccountForm ->
      CAF.isComplete model.createAccountForm

    CreateEventForm ->
      CEF.isComplete model.createEventForm

    GuestList ->
      True

    Summary ->
      True



-- VIEW


view dispatcher model =
  let
    forwardWith =
      Signal.forwardTo dispatcher

    header page =
      case page of
        CreateAccountForm ->
          text "Hi! Tell me a little bit about yourself!"

        CreateEventForm ->
          text "Hi! Tell me a little bit about your event."

        GuestList ->
          text "Who's invited?"

        Summary ->
          text "Please review your information."

    curPage page =
      case page of
        CreateAccountForm ->
          CAF.view (forwardWith UpdateCreateAccountForm) model.createAccountForm

        CreateEventForm ->
          CEF.view (forwardWith UpdateCreateEventForm) model.createEventForm

        GuestList ->
          GL.view (forwardWith UpdateGuestList) model.guestList

        Summary ->
          S.view (forwardWith UpdateCreateEventForm) model.createEventForm (forwardWith UpdateGuestList) model.guestList
  in
    -- Html.form [ autocomplete True ]
    Html.div
      [ class "container", autocomplete True ]
      [ -- Header
        div
          [ class "row" ]
          [ h3 [] [ header model.pages.current ] ]
        -- Main content
      , curPage model.pages.current
        -- Buttons
      , div
          [ class "row" ]
          [ button
              [ class "waves-effect waves-light btn col s2"
              , onClick dispatcher PrevPage
              ]
              [ text "Prev" ]
          , button
              [ class "waves-effect waves-light btn col s2 offset-s8"
              , onClick dispatcher NextPage
              ]
              [ text "Next" ]
          ]
      ]
