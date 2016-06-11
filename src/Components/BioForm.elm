module Components.BioForm exposing (..)

import Regex
import Html exposing (..)
import Html.App exposing (map)
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
  { employer = validatedField "Who's your employer? (optional)" "text" alwaysValid
  , jobTitle = validatedField "What's your job title? (optional)" "text" alwaysValid
  , birthday = validatedField "When's your birthday? (optional)" "date" alwaysValid
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


view : Model -> Html Action
view model =
--  let
--    contramapWith =
--      Signal.forwardTo dispatcher
--  in
    div
      []
      [ map SetEmployer (F.view model.employer)
      , map SetJobTitle (F.view model.jobTitle)
      , map SetBirthday (F.view model.birthday)
      ]
