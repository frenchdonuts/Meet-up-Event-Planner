module Components.CreateEventForm exposing (..)

import List as L
import Date exposing (fromString, toTime)
import Regex
import Task
import Http
import Json.Decode as Json
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)
import Components.Field as F exposing (..)
import Validate exposing (..)
import Components.LocationSearcher as LS exposing (..)



-- MODEL
type alias Model =
  { name : Field
  , type' : Field
  , host : Field
  , location : LS.Model
  , startTime : Field
  , endTime : Field
  , optMsg : String
  , startTimeLTendTimeValidator : Validator String ( Field, Field )
  , errMsgs : List String
  }


init : (Model, Cmd Action)
init =
  let
    name' =
      validatedField "What do you want to call this event?" "text" (ifBlank "Required")

    errPredicate ( startTimeField, endTimeField ) =
      let
        startTime =
          -- Result data type - also known as Either in other languages
          Result.map Date.toTime (Date.fromString startTimeField.value)

        endTime =
          Result.map Date.toTime (Date.fromString endTimeField.value)
      in
        case (Result.map2 (>) startTime endTime) of
          Ok isStartTimeGreaterThanEndTime ->
            isStartTimeGreaterThanEndTime

          Err _ ->
            False
    (lsModel, lsCmd) = LS.init
  in
    ( { name =
        { name' | autofocus = True }
    , type' =
        validatedField "What type of event is it?" "text" (ifBlank "Required")
    , host =
        validatedField "Who's hosting this event?" "text" (ifBlank "Required")
    , location = lsModel
    , startTime =
        validatedField "When does it start?" "datetime-local" (timeValidator "Required")
    , endTime =
        validatedField "When does it end?" "datetime-local" (timeValidator "Required")
    , optMsg = ""
    , startTimeLTendTimeValidator =
        ifInvalid errPredicate "An event must start before it ends! Hint: Either push the start time earlier or the end time later."
    , errMsgs = []
    }, Cmd.batch
        [ Cmd.map UpdateLocationInput lsCmd ]
    )


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
    && LS.isComplete model.location
    && fieldIsValid model.startTime
    && fieldIsValid model.endTime
    && List.isEmpty model.errMsgs



-- UPDATE


type Action
  = UpdateNameInput F.Action
  | UpdateTypeInput F.Action
  | UpdateHostInput F.Action
  | UpdateLocationInput LS.Msg
  | UpdateStartTimeInput F.Action
  | UpdateEndTimeInput F.Action
  | UpdateOptMsgInput String


update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    UpdateNameInput a ->
      ( { model | name = F.update a model.name }, Cmd.none )

    UpdateTypeInput a ->
      ( { model | type' = F.update a model.type' }, Cmd.none )

    UpdateHostInput a ->
      ( { model | host = F.update a model.host }, Cmd.none )

    UpdateLocationInput a ->
      let
        (lsModel, lsCmd) = LS.update a model.location
      in
        ( { model | location = lsModel }, Cmd.map UpdateLocationInput lsCmd )

    UpdateStartTimeInput a ->
      let
        updatedStartTime =
          F.update a model.startTime
      in
        ( { model
          | startTime = updatedStartTime
          , errMsgs = model.startTimeLTendTimeValidator ( updatedStartTime, model.endTime )
        }, Cmd.none )

    UpdateEndTimeInput a ->
      let
        updatedEndTime =
          F.update a model.endTime
      in
        ( { model
          | endTime = updatedEndTime
          , errMsgs = model.startTimeLTendTimeValidator ( model.startTime, updatedEndTime )
        }, Cmd.none )

    UpdateOptMsgInput msg ->
      ( { model | optMsg = msg }, Cmd.none )

-- VIEW


view : Model -> Html Action
view model =
    div
      [ class "row" ]
      [ map UpdateNameInput (F.view model.name)
      , map UpdateTypeInput (F.view model.type')
      , div [ class "row" ] []
      , div [ class "row" ] []
      , map UpdateHostInput (F.view model.host)
      , map UpdateLocationInput (LS.view model.location)
      , div [ class "row" ] []
      , div [ class "row" ] []
      , map UpdateStartTimeInput (F.view model.startTime)
      , map UpdateEndTimeInput (F.view model.endTime)
      , div [ class "row" ] []
      , div [ class "row" ] []
      , textarea_ model.optMsg
      , ul [ style [ ( "color", "#F44336" ) ] ] (List.map (\errMsg -> li [] [ text errMsg ]) model.errMsgs)
      ]


textarea_ optMsg =
  div
    [ class "input-field col s12" ]
    [ textarea
        [ id "optional-msg"
        , class "materialize-textarea"
        , placeholder ""
        , onInput UpdateOptMsgInput
        ]
        [ text optMsg ]
    , label [ for "optional-msg", class "active" ] [ text "Message to your guests (optional)" ]
    ]
