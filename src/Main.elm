port module Main exposing (..)

import Debug
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)
import List as L
import Focus exposing (..)



import Components.TwoSelectionListNavigator exposing (..)
import Components.SelectionList as SL
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)
import Components.CreateAccountForm as CAF exposing (..)
import Components.Summary as S exposing (..)
import Components.CreatedEventsPage as CEP exposing (..)


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
  | EventsCreated

fnOfPage : a -> a -> a -> a -> a -> Page -> a
fnOfPage onCreateAccountForm onCreateEventForm onGuestList onSummary onEventsCreated page =
  case page of
    CreateAccountForm ->
      onCreateAccountForm

    CreateEventForm ->
      onCreateEventForm

    GuestList ->
      onGuestList

    Summary ->
      onSummary

    EventsCreated ->
      onEventsCreated

type alias Model =
  { createAccountForm : CAF.Model
  , curEventCreationFlow : EventCreationFlow
  , createdEventsPage : CEP.Model
  , pages : TwoSelectionListNavigator Page
  }

type alias EventCreationFlow =
  { createEventForm : CEF.Model
  , guestList : GL.Model
  }

-- FOCI

createEventForm : Focus { r | createEventForm : a } a
createEventForm =
  create .createEventForm (\f r -> { r | createEventForm = f r.createEventForm })

guestList : Focus { r | guestList : a } a
guestList =
  create .guestList (\f r -> { r | guestList = f r.guestList })

curEventCreationFlow : Focus { r | curEventCreationFlow : a } a
curEventCreationFlow =
  create .curEventCreationFlow (\f r -> { r | curEventCreationFlow = f r.curEventCreationFlow })


-- INIT
init : (Model, Cmd Action)
init =
  let
    (eventCreationFlowModel, eventCreationFlowCmd) =
      newEventCreationFlow
  in
    ({ curEventCreationFlow = eventCreationFlowModel
     , createdEventsPage = CEP.init
     , createAccountForm = CAF.init
     , pages = newNavigator flow1 flow2
     }
     , Cmd.batch
        [ eventCreationFlowCmd ]
     )

newEventCreationFlow =
  let
    (cefModel, cefCmd) = CEF.init
  in
    ({ createEventForm = cefModel
     , guestList = GL.init
     }
    , Cmd.batch
        [ Cmd.map UpdateCreateEventForm cefCmd ]
    )
    
flow1 = SL.newSelectionList
          CreateAccountForm
          [ CreateEventForm, GuestList, Summary ]

flow2 = SL.newSelectionList
          EventsCreated
          []



-- UPDATE


type Action
  = NoOp
  | UpdateCreateEventForm CEF.Action
  | UpdateGuestList GL.Action
  | UpdateCreateAccountForm CAF.Action
  | UpdateCreatedEventsPage CEP.Msg
  | NextPage
  | PrevPage
  | FinishEventCreation EventCreationFlow
  | NavigateToOtherFlow


update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      ( model, Cmd.none )

    UpdateCreateEventForm a ->
      let
        (cefModel, cefCmd) =
          CEF.update a model.curEventCreationFlow.createEventForm
      in
        ( set (curEventCreationFlow => createEventForm) cefModel model
        , Cmd.map UpdateCreateEventForm cefCmd )

    UpdateGuestList a ->
      let
        newGLModel = GL.update a model.curEventCreationFlow.guestList
      in
        ( set (curEventCreationFlow => guestList) newGLModel model
        , Cmd.none )

    UpdateCreateAccountForm a ->
      ( { model
        | createAccountForm = CAF.update a model.createAccountForm
        }
      , Cmd.none )

    UpdateCreatedEventsPage m ->
      ( { model
        | createdEventsPage = CEP.update m model.createdEventsPage
        }
      , Cmd.none)

    NextPage ->
      if curPageIsValid (current model.pages) model then
        ( { model | pages = forward model.pages }, focusOnFirstInputAboveFold "" )
      else
        ( model, Cmd.none )

    PrevPage ->
      ( { model | pages = back model.pages }, focusOnFirstInputAboveFold "" )

    FinishEventCreation eventCreationFlow ->
      ( { model
        | createdEventsPage = CEP.update (AddEventCreationFlow eventCreationFlow) model.createdEventsPage
        }
      , Cmd.none )

    NavigateToOtherFlow ->
      ( { model
        | pages = toggle model.pages
        }
      , Cmd.none )


curPageIsValid : Page -> Model -> Bool
curPageIsValid page model =
  case page of
    CreateAccountForm ->
      CAF.isComplete model.createAccountForm

    CreateEventForm ->
      CEF.isComplete model.curEventCreationFlow.createEventForm

    GuestList ->
      GL.isComplete model.curEventCreationFlow.guestList

    Summary ->
      CEF.isComplete model.curEventCreationFlow.createEventForm

    EventsCreated ->
      True



-- VIEW

view : Model -> Html Action
view model =
  let
    header =
      fnOfPage
        (text "Hi! Let's get you registered first.")
        (text "I'll need some information about your event...")
        (text "Who's invited?")
        (text "Please review your information.")
        (text "All Events Created")

    curPageView =
      fnOfPage
        (Html.map UpdateCreateAccountForm (CAF.view model.createAccountForm))
        (Html.map UpdateCreateEventForm (CEF.view model.curEventCreationFlow.createEventForm))
        (Html.map UpdateGuestList (GL.view model.curEventCreationFlow.guestList))
        (Html.map toMainAction (S.view model.curEventCreationFlow.createEventForm model.curEventCreationFlow.guestList))
        (Html.map UpdateCreatedEventsPage (CEP.view model.createdEventsPage))

    toMainAction summaryAction =
      case summaryAction of
        S.UpdateCreateEventForm a -> UpdateCreateEventForm a
        S.UpdateGuestList a -> UpdateGuestList a

    visible =
      [ ( "visibility", "visible" ) ]

    hidden =
      [ ( "visibility", "hidden" ) ]

    prevBtnStyle =
      fnOfPage hidden visible visible visible hidden

    nextBtnStyle =
      fnOfPage visible visible visible visible hidden

    nextBtnText =
      fnOfPage "Next" "Next" "Next" "Finish" ""

    nextBtnMsg =
      fnOfPage NextPage NextPage NextPage (FinishEventCreation model.curEventCreationFlow) NoOp

    navItem txt isEventsCreated =
      let
        navItemClass =
          case (current model.pages) of
            EventsCreated -> if isEventsCreated then "active" else ""
            _ -> if (not isEventsCreated) then "active" else ""
      in
        li
          [ class navItemClass
          , onClick NavigateToOtherFlow
          ]
          [ a [] [ text txt ] ]

    currentPage =
      current model.pages
  in
    div
      []
      [ nav
          [ class "top-nav" ]
          [ div
              [ class "nav-wrapper" ]
              [ ul
                  [ class "left"]
                  (L.map
                    (\(txt, isEventsCreated) -> navItem txt isEventsCreated)
                    [ ("Create Events", False)
                    , ("Events Created", True)
                    ]
                  )
              ]
          ]
      , Html.form
          [ class "container", autocomplete True ]
          [ -- Header
            div
              [ class "row" ]
              [ h3 [] [ header currentPage ] ]
            -- Main content
          , curPageView currentPage
            -- Buttons
          , div
              [ class "row" ]
              [ button
                  [ type' "button"
                  , class "waves-effect waves-light btn col s2"
                  , style (prevBtnStyle currentPage)
                  , onClick PrevPage
                  ]
                  [ text "Prev" ]
              , button
                  [ type' "button"
                  , class "waves-effect waves-light btn col s2 offset-s8"
                  , style (nextBtnStyle currentPage)
                  , onClick (nextBtnMsg currentPage)
                  ]
                  [ text (nextBtnText currentPage) ]
              ]
          ]
      ]
