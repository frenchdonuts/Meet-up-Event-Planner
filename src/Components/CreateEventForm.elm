module Components.CreateEventForm (..) where

import List exposing (..)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)
import Validate exposing (..)


{-

Name of the event
Type of the event (birthday party, conference talk, wedding, etc.)
Event host (could be an individual’s name or an organization)
Event start date and time
Event end date and time
Guest list
Location
Optional message to the guests with additional information about the event

-}
-- MODEL


type alias Model =
  { name : FI.Model
  , type' : FI.Model
  , host : FI.Model
  , startTime : FI.Model
  , endTime : FI.Model
  , location : FI.Model
  , optMsg : String
  }


type alias Event =
  Model



-- What kind of form-element will I provide to allow Users to put in Guest lists?


init : Model
init =
  { name = FI.init "Event Name: " ""
  , type' = FI.init "Event Type: " ""
  , host = FI.init "Event Host: " ""
  , startTime = FI.init "Start Time: " ""
  , endTime = FI.init "End Time: " ""
  , location = FI.init "Location: " ""
  , optMsg = ""
  }



-- UPDATE


type Action
  = UpdateNameInput FI.Action
  | UpdateTypeInput FI.Action
  | UpdateHostInput FI.Action
  | UpdateStartTimeInput FI.Action
  | UpdateEndTimeInput FI.Action
  | UpdateLocationInput FI.Action
  | UpdateOptMsgInput String


update : Action -> Model -> Model
update action model =
  case action of
    UpdateNameInput a ->
      { model | name = FI.update a model.name }

    UpdateTypeInput a ->
      { model | type' = FI.update a model.type' }

    UpdateHostInput a ->
      { model | host = FI.update a model.host }

    UpdateStartTimeInput a ->
      { model | startTime = FI.update a model.startTime }

    UpdateEndTimeInput a ->
      { model | endTime = FI.update a model.endTime }

    UpdateLocationInput a ->
      { model | location = FI.update a model.location }

    UpdateOptMsgInput msg ->
      { model | optMsg = msg }



-- VIEW


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    div
      [ class "row valign" ]
      [ div
          [ class "section" ]
          [ h5 [] [ text "" ]
          , div
              [ class "row" ]
              [ FI.text_ (contramapWith UpdateNameInput) model.name (ifBlank "Please give this event a name.")
              , FI.text_ (contramapWith UpdateTypeInput) model.type' (ifBlank "What kind of event is it?")
              ]
          , div [ class "row" ] []
          , div [ class "row" ] []
          , div
              [ class "row" ]
              [ FI.text_ (contramapWith UpdateHostInput) model.host (ifBlank "Who's the host?")
              , FI.text_ (contramapWith UpdateLocationInput) model.location (ifBlank "Where is the event going to be held?")
              ]
          , div [ class "row" ] []
          , div [ class "row" ] []
          , div
              [ class "row" ]
              [ FI.datetime_ (contramapWith UpdateStartTimeInput) model.startTime
              , FI.datetime_ (contramapWith UpdateEndTimeInput) model.endTime
              ]
          , div
              [ class "row" ]
              [ textarea_ dispatcher model.optMsg ]
          ]
      ]



-- Two problems that might be connected:
--   Validation based on TWO inputs: StartTime < EndTime
--   Checking if every single input is VALID
-- Let's see if we can/should move validation to the UPDATE step
-- The only reasonable solution is to move Validation logic up to this component (CreateEventForm).


textarea_ dispatcher optMsg =
  div
    [ class "input-field col s12" ]
    [ textarea
        [ id "optional-msg"
        , class "materialize-textarea"
        , placeholder ""
        , on "input" targetValue (\str -> Signal.message dispatcher (UpdateOptMsgInput str))
        ]
        [ text optMsg ]
    , label [ for "optional-msg" ] [ text "Message to your guests (optional)" ]
    ]
