module Components.Summary (..) where

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Components.CreateEventForm as CEF exposing (..)
import Components.GuestList as GL exposing (..)


view : Signal.Address CEF.Action -> CEF.Model -> Signal.Address GL.Action -> GL.Model -> Html
view cefDispatcher cefModel glDispatcher glModel =
  div
    [ class "row" ]
    [ CEF.view cefDispatcher cefModel
    , GL.view glDispatcher glModel
    ]


{-|
view : CEF.Model -> GL.Model -> Html
view eventInfo guestList =
  div
    [ class "row" ]
    [ div
        [ class "row" ]
        [ div
            [ class "section" ]
            [ div
                [ class "row" ]
                [ F.disabledView eventInfo.name
                , F.disabledView eventInfo.type'
                ]
            , div [ class "row" ] []
            , div [ class "row" ] []
            , div
                [ class "row" ]
                [ F.disabledView eventInfo.host
                , F.disabledView eventInfo.location
                ]
            , div [ class "row" ] []
            , div [ class "row" ] []
            , div
                [ class "row" ]
                [ F.disabledView eventInfo.startTime
                , F.disabledView eventInfo.endTime
                ]
            , div [ class "row" ] []
            , div [ class "row" ] []
            , div
                [ class "row" ]
                [ textarea_ eventInfo.optMsg ]
            ]
        ]
    ]
-}
textarea_ optMsg =
  div
    [ class "input-field col s12" ]
    [ textarea
        [ id "optional-msg"
        , class "materialize-textarea"
        , placeholder ""
        , disabled True
        ]
        [ text optMsg ]
    , label [ for "optional-msg" ] [ text "Message to your guests (optional)" ]
    ]


{-|
guestItem : Signal.Address Action -> ( ID, String ) -> Html
guestItem dispatcher ( id, name ) =
  li
    [ class "collection-item row" ]
    [ div
        []
        [ text name
        , span
            [ class "secondary-content" ]
            [ i
                [ class "material-icons"
                , onClick dispatcher (RemoveGuest id)
                ]
                [ text "close" ]
            ]
        ]
    ]
-}
