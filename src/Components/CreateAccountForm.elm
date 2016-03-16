module Components.CreateAccountForm (..) where

import Html exposing (..)
import Html.Attributes exposing (id, value, target, type', for)
import Html.Events exposing (..)
import Components.FormInput as FI exposing (..)
import Components.BioForm as Bio exposing (..)


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
  { name = FI.withLabel "Name: "
  , emailAddress = FI.withLabel "Email: "
  , password = FI.withLabel "Password: "
  , bio = Bio.init
  }


type Action
  = SET_NAME FI.Action
  | SET_EMAIL_ADDRESS FI.Action
  | SET_PASSWORD FI.Action
  | SET_BIO Bio.Action


update : Action -> Model -> Model
update action model =
  case action of
    SET_NAME fiAction ->
      { model | name = FI.update fiAction model.name }

    SET_EMAIL_ADDRESS fiAction ->
      { model | emailAddress = FI.update fiAction model.emailAddress }

    SET_PASSWORD fiAction ->
      { model | password = FI.update fiAction model.password }

    SET_BIO bioAction ->
      { model | bio = Bio.update bioAction model.bio }


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    form
      []
      [ h1 [] [ text "Let's create your account!" ]
      , FI.text_ (contramapWith SET_NAME) model.name
      , FI.text_ (contramapWith SET_EMAIL_ADDRESS) model.emailAddress
      , FI.password_ (contramapWith SET_PASSWORD) model.password
      , Bio.view (contramapWith SET_BIO) model.bio
      ]
