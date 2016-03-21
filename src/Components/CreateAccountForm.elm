module Components.CreateAccountForm (..) where

import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)
import Components.BioForm as Bio exposing (..)
import Validate exposing (..)


{-
Account creation should include, but is not limited to:

Name
Email address
Secure password (with character and length requirements)
Optional public biographical information (such as employer, job title, birthday, etc)

-}


type alias Model =
  { name : FI.Model
  , emailAddress : FI.Model
  , password : FI.Model
  , bio : Bio.Model
  }


init : Model
init =
  { name = FI.init "Name: " ""
  , emailAddress = FI.init "Email: " ""
  , password = FI.init "Password: " ""
  , bio = Bio.init
  }


type Action
  = UpdateNameInput FI.Action
  | UpdateEmailAddressInput FI.Action
  | UpdatePasswordInput FI.Action
  | UpdateBioForm Bio.Action


update : Action -> Model -> Model
update action model =
  case action of
    UpdateNameInput fiAction ->
      { model | name = FI.update fiAction model.name }

    UpdateEmailAddressInput fiAction ->
      { model | emailAddress = FI.update fiAction model.emailAddress }

    UpdatePasswordInput fiAction ->
      { model | password = FI.update fiAction model.password }

    UpdateBioForm bioAction ->
      { model | bio = Bio.update bioAction model.bio }


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher

    emailValidator =
      all
        [ ifBlank "Please put in your email."
        , ifInvalidEmail "Please put in a valid email."
        ]
  in
    div
      [ class "container" ]
      [ h1 [] [ text "One last thing..." ]
      , div
          [ class "row" ]
          [ FI.text_ (contramapWith UpdateNameInput) model.name (ifBlank "Please put in your name.")
          , FI.text_ (contramapWith UpdateEmailAddressInput) model.emailAddress emailValidator
          ]
      , div
          [ class "row" ]
          [ FI.password_ (contramapWith UpdatePasswordInput) model.password (ifBlank "Please put in a password.") ]
      , Bio.view (contramapWith UpdateBioForm) model.bio
      ]
