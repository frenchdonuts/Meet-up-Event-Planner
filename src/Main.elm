port module Main exposing (..)

import Debug
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)


-- official 'Elm Architecture' package
-- https://github.com/evancz/start-app

import Components.SelectionList exposing (..)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)
import Components.CreateAccountForm as CAF exposing (..)
import Components.Summary as S exposing (..)


-- APP KICK OFF!
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- Ports
port focusOnFirstInputAboveFold : String -> Cmd msg


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
  , onEventsCreatedPage : Bool
  }



-- INIT

pages =
  { previous = [ CreateAccountForm ]
  , current = CreateEventForm
  , next = [ GuestList, Summary ]
  }

--pages = newSelectionList CreateAccountForm [ CreateEventForm, GuestList, Summary, Thanks ]

init : (Model, Cmd Action)
init =
  let
    -- : (CEF.Model, Cmd CEF.Action)
    (cefModel, cefCmds) =
      CEF.init
  in
    ({ createEventForm = cefModel
     , guestList = GL.init
     , createAccountForm = CAF.init
     , pages = pages
     , onEventsCreatedPage = False
     }
     , Cmd.batch
        [ Cmd.map UpdateCreateEventForm cefCmds ]
     )



-- UPDATE


type Action
  = NoOp
  | UpdateCreateEventForm CEF.Action
  | UpdateGuestList GL.Action
  | UpdateCreateAccountForm CAF.Action
  | NextPage
  | PrevPage


update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      ( model, Cmd.none )

    UpdateCreateEventForm a ->
      let
        (cefModel, cefCmd) =
          CEF.update a model.createEventForm
      in
        ( { model | createEventForm = cefModel }, Cmd.map UpdateCreateEventForm cefCmd )

    UpdateGuestList a ->
      ( { model | guestList = GL.update a model.guestList }, Cmd.none )

    UpdateCreateAccountForm a ->
      ( { model | createAccountForm = CAF.update a model.createAccountForm }, Cmd.none )

    NextPage ->
      if curPageIsValid model.pages.current model then
        ( { model | pages = forward model.pages }, focusOnFirstInputAboveFold "" )
      else
        ( model, Cmd.none )

    PrevPage ->
      ( { model | pages = back model.pages }, focusOnFirstInputAboveFold "" )


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
      CEF.isComplete model.createEventForm



-- VIEW


view model =
  let
    header =
      fnOfPage
        (text "Hi! Let's get you registered first.")
        (text "I'll need some information about your event...")
        (text "Who's invited?")
        (text "Please review your information.")

    curPage =
      fnOfPage
        (Html.map UpdateCreateAccountForm (CAF.view model.createAccountForm))
        (Html.map UpdateCreateEventForm (CEF.view model.createEventForm))
        (Html.map UpdateGuestList (GL.view model.guestList))
        (Html.map toMainAction (S.view model.createEventForm model.guestList))

    toMainAction summaryAction =
      case summaryAction of
        S.UpdateCreateEventForm a -> UpdateCreateEventForm a
        S.UpdateGuestList a -> UpdateGuestList a

    visible =
      [ ( "visibility", "visible" ) ]

    hidden =
      [ ( "visibility", "hidden" ) ]

    prevBtnStyle =
      fnOfPage hidden visible visible visible

    nextBtnStyle =
      fnOfPage visible visible visible visible

    nextBtnText =
      fnOfPage "Next" "Next" "Next" "Finish"

    nextBtnType =
      fnOfPage "button" "button" "button" "submit"
  in
    div
      []
      [ nav [ class "top-nav" ] [ div [ class "nav-wrapper" ] [] ]
        -- Html.form [ autocomplete True ]
      , Html.form
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
                  [ type' (nextBtnType model.pages.current)
                  , class "waves-effect waves-light btn col s2"
                  , style (prevBtnStyle model.pages.current)
                  , onClick PrevPage
                  ]
                  [ text "Prev" ]
              , button
                  [ type' "button"
                  , class "waves-effect waves-light btn col s2 offset-s8"
                  , style (nextBtnStyle model.pages.current)
                  , onClick NextPage
                  ]
                  [ text (nextBtnText model.pages.current) ]
              ]
          ]
      ]


fnOfPage : a -> a -> a -> a -> Page -> a
fnOfPage onCreateAccountForm onCreateEventForm onGuestList onSummary page =
  case page of
    CreateAccountForm ->
      onCreateAccountForm

    CreateEventForm ->
      onCreateEventForm

    GuestList ->
      onGuestList

    Summary ->
      onSummary
