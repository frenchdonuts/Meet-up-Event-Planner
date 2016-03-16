module Components.CreateEventForm (..) where

import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)


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
  , optMsg : FI.Model
  }


type alias Event =
  Model



-- What kind of form-element will I provide to allow Users to put in Guest lists?


init : Model
init =
  { name = FI.withLabel "Event Name: "
  , type' = FI.withLabel "Event Type: "
  , host = FI.withLabel "Event Host: "
  , startTime = FI.withLabel "Start Time: "
  , endTime = FI.withLabel "End Time: "
  , location = FI.withLabel "Location: "
  , optMsg = FI.withLabel "Optional message: "
  }



-- UPDATE


type Action
  = SetName FI.Action
  | SetType FI.Action
  | SetHost FI.Action
  | SetStartTime FI.Action
  | SetEndTime FI.Action
  | SetLocation FI.Action
  | SetOptMsg FI.Action


update : Action -> Model -> Model
update action model =
  case action of
    SetName a ->
      { model | name = FI.update a model.name }

    SetType a ->
      { model | type' = FI.update a model.type' }

    SetHost a ->
      { model | host = FI.update a model.host }

    SetStartTime a ->
      { model | startTime = FI.update a model.startTime }

    SetEndTime a ->
      { model | endTime = FI.update a model.endTime }

    SetLocation a ->
      { model | location = FI.update a model.location }

    SetOptMsg a ->
      { model | optMsg = FI.update a model.optMsg }



-- VIEW


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    form
      []
      [ h1 [] [ text "Hi! Tell me about your event!" ]
      , FI.text_ (contramapWith SetName) model.name
      , FI.text_ (contramapWith SetType) model.type'
      , FI.text_ (contramapWith SetHost) model.host
      , FI.datetime_ (contramapWith SetStartTime) model.startTime
      , FI.datetime_ (contramapWith SetEndTime) model.endTime
      , FI.text_ (contramapWith SetLocation) model.location
      , FI.text_ (contramapWith SetOptMsg) model.optMsg
      ]


guestView : String -> Html
guestView guest =
  li [] [ text guest ]
