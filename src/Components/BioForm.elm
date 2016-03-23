module Components.BioForm (..) where

import Regex
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Components.Field as F exposing (..)
import Validate exposing (..)


-- MODEL


type alias Model =
  { employer : Field
  , jobTitle : Field
  , birthday : Field
  }


init =
  { employer = validatedField "Employer (optional): " "text" alwaysValid
  , jobTitle = validatedField "Job Title (optional): " "text" alwaysValid
  , birthday = validatedField "Birthday (optional): " "date" alwaysValid
  }


isComplete : Model -> Bool
isComplete model =
  fieldIsValid model.employer
    && fieldIsValid model.jobTitle
    && fieldIsValid model.birthday



-- UPDATE


type Action
  = SetEmployer F.Action
  | SetJobTitle F.Action
  | SetBirthday F.Action


update : Action -> Model -> Model
update action model =
  let
    { employer, jobTitle, birthday } =
      model
  in
    case action of
      SetEmployer a ->
        { model | employer = F.update a employer }

      SetJobTitle a ->
        { model | jobTitle = F.update a jobTitle }

      SetBirthday a ->
        { model | birthday = F.update a birthday }



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
          [ F.view (contramapWith SetEmployer) model.employer
          , F.view (contramapWith SetJobTitle) model.jobTitle
          ]
      , div
          [ class "row" ]
          [ F.view (contramapWith SetBirthday) model.birthday ]
      ]
