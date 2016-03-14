module Components.BioForm (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Components.FormInput as FI exposing (..)


-- MODEL


type alias Model =
  { employer : FI.Model
  , jobTitle : FI.Model
  , birthday : FI.Model
  }


init =
  { employer = FI.withLabel "Employer: "
  , jobTitle = FI.withLabel "Job Title: "
  , birthday = FI.withLabel "Birthday: "
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
      [ FI.text_ (contramapWith SetEmployer) model.employer
      , FI.text_ (contramapWith SetJobTitle) model.jobTitle
      , FI.date_ (contramapWith SetBirthday) model.birthday
      ]
