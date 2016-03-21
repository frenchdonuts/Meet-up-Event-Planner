module Components.BioForm (..) where

import Html exposing (..)
import Html.Attributes as A exposing (..)
import Components.FormInput as FI exposing (..)


-- MODEL


type alias Model =
  { employer : FI.Model
  , jobTitle : FI.Model
  , birthday : FI.Model
  }


init =
  { employer = FI.init "Employer (optional): " ""
  , jobTitle = FI.init "Job Title (optional): " ""
  , birthday = FI.init "Birthday (optional): " ""
  }


type alias Date =
  { month : Int
  , day : Int
  , year : Int
  }


defaultDate =
  { month = 0
  , day = 0
  , year = 0
  }



-- UPDATE


type Action
  = SetEmployer FI.Action
  | SetJobTitle FI.Action
  | SetBirthday FI.Action


update : Action -> Model -> Model
update action model =
  let
    { employer, jobTitle, birthday } =
      model
  in
    case action of
      SetEmployer a ->
        { model | employer = FI.update a employer }

      SetJobTitle a ->
        { model | jobTitle = FI.update a jobTitle }

      SetBirthday a ->
        { model | birthday = FI.update a birthday }



-- VIEW


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    div
      []
      [ div
          [ class "row" ]
          [ FI.text_ (contramapWith SetEmployer) model.employer FI.alwaysValid
          , FI.text_ (contramapWith SetJobTitle) model.jobTitle FI.alwaysValid
          ]
      , div
          [ class "row" ]
          [ FI.date_ (contramapWith SetBirthday) model.birthday ]
      ]
