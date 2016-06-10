module Components.CreatedEventsPage exposing (..)


import Html as H exposing (..)
import Html.App exposing (map)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import List as L
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)
import Components.Summary as S exposing (..)

-- MODEL
type alias Model =
  { completedEventCreationFlows : List EventCreationFlow }

type alias EventCreationFlow =
  { createEventForm : CEF.Model
  , guestList : GL.Model
  }

init : Model
init =
  { completedEventCreationFlows = [] }
-- UPDATE
type Msg
  = NoOp S.Action
  | AddEventCreationFlow EventCreationFlow

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp _ -> model

    AddEventCreationFlow eventCreationFlow ->
      let
        completedEventCreationFlows =
          model.completedEventCreationFlows ++ [ eventCreationFlow ]
      in
        { model
        | completedEventCreationFlows = completedEventCreationFlows
        }

-- VIEW
view : Model -> Html Msg
view model =
  let
    hide =
      L.isEmpty model.completedEventCreationFlows
  in
    div
      [ hidden hide ]
      ( L.map (\flow -> itemView flow) model.completedEventCreationFlows )

itemView : EventCreationFlow -> Html Msg
itemView eventCreationFlow =
  let
    { createEventForm, guestList } = eventCreationFlow
  in
    div
      []
      [ map NoOp (S.view createEventForm guestList)
      , hr [ style [ ("border-top", "1px solid #8c8b8b") ] ] []
      -- Extra padding between events.
      , div [ class "row" ] []
      , div [ class "row" ] []
      ]
