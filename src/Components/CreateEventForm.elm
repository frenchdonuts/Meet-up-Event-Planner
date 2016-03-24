module Components.CreateEventForm (..) where

import List exposing (..)
import Regex
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)
import Components.Field as F exposing (..)
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
  { name : Field
  , type' : Field
  , host : Field
  , location : Field
  , startTime : Field
  , endTime : Field
  , optMsg : String
  }


type alias Event =
  Model


init : Model
init =
  { name =
      validatedField "Event Name: " "text" (ifBlank "Please give this event a name.")
  , type' =
      validatedField "Event Type: " "text" (ifBlank "What kind of event is it?")
  , host =
      validatedField "Event Host: " "text" (ifBlank "Who's the host?")
  , location =
      validatedField "Location: " "text" (ifBlank "Where is the event going to be held?")
  , startTime =
      validatedField "Start Time: " "datetime-local" (timeValidator "When is it goin to start?")
  , endTime =
      validatedField "End Time: " "datetime-local" (timeValidator "When will it end?")
  , optMsg = ""
  }


timeValidator : String -> Validator String String
timeValidator =
  let
    validTime =
      Regex.regex "\\d{4}-\\d{2}-\\d{2}"
  in
    ifInvalid (\timestr -> not <| Regex.contains validTime timestr)


isComplete : Model -> Bool
isComplete model =
  fieldIsValid model.name
    && fieldIsValid model.type'
    && fieldIsValid model.host
    && fieldIsValid model.location
    && fieldIsValid model.startTime
    && fieldIsValid model.endTime



-- UPDATE


type Action
  = UpdateNameInput F.Action
  | UpdateTypeInput F.Action
  | UpdateHostInput F.Action
  | UpdateLocationInput F.Action
  | UpdateStartTimeInput F.Action
  | UpdateEndTimeInput F.Action
  | UpdateOptMsgInput String


update : Action -> Model -> Model
update action model =
  case action of
    UpdateNameInput a ->
      { model | name = F.update a model.name }

    UpdateTypeInput a ->
      { model | type' = F.update a model.type' }

    UpdateHostInput a ->
      { model | host = F.update a model.host }

    UpdateLocationInput a ->
      { model | location = F.update a model.location }

    UpdateStartTimeInput a ->
      { model | startTime = F.update a model.startTime }

    UpdateEndTimeInput a ->
      { model | endTime = F.update a model.endTime }

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
      [ class "row" ]
      [ div
          [ class "row" ]
          [ F.view (contramapWith UpdateNameInput) model.name
          , F.view (contramapWith UpdateTypeInput) model.type'
          ]
      , div [ class "row" ] []
      , div [ class "row" ] []
      , div
          [ class "row" ]
          [ F.view (contramapWith UpdateHostInput) model.host
          , F.view (contramapWith UpdateLocationInput) model.location
          ]
      , div [ class "row" ] []
      , div [ class "row" ] []
      , div
          [ class "row" ]
          [ F.view (contramapWith UpdateStartTimeInput) model.startTime
          , F.view (contramapWith UpdateEndTimeInput) model.endTime
          ]
      , div [ class "row" ] []
      , div [ class "row" ] []
      , div
          [ class "row" ]
          [ textarea_ dispatcher model.optMsg ]
      ]


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
    , label [ for "optional-msg", class "active" ] [ text "Message to your guests (optional)" ]
    ]
