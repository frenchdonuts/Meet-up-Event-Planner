module Components.CreateAccountForm (..) where

import String
import Regex
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Components.Field as F exposing (..)
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
-- MODEL


type alias Model =
  { name : Field
  , emailAddress : Field
  , password : Field
  , bio : Bio.Model
  }


init : Model
init =
  let name' = validatedField "Name: " "text" (ifBlank "What's your name?")
  in
    { name = { name' | autofocus = True }
    , emailAddress = validatedField "Email: " "text" (ifInvalidEmail "Please put in a valid email.")
    , password = validatedField "Password: " "password" ifInvalidPassword
    , bio = Bio.init
    }


ifInvalidPassword : Validator String String
ifInvalidPassword =
  let
    hasPattern pattern err =
      ifInvalid (\str -> not <| Regex.contains (Regex.regex pattern) str) err

    hasSymbols' =
      hasPattern "\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*"

    hasSymbols =
      hasPattern "\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*" "Password must contain at least one of these symbols: !, @, #, $, %, ^, &, *"

    hasNumbers =
      hasPattern "\\d" "Password must contain at least one number"

    hasLowerCase =
      hasPattern "[a-z]" "Password must contain at least one lowercase letter"

    hasUpperCase =
      hasPattern "[A-Z]" "Password must contain at least one uppercase letter"

    isLongerThan8Chars =
      ifInvalid (\str -> not <| String.length str >= 8) "Password must be over 8 characters"

    isLessThan100Chars =
      ifInvalid (\str -> not <| String.length str < 100) "That's a ridiculous number of characters"
  in
    Validate.all
      [ hasSymbols
      , hasNumbers
      , hasLowerCase
      , hasUpperCase
      , isLongerThan8Chars
      , isLessThan100Chars
      ]


isComplete : Model -> Bool
isComplete model =
  fieldIsValid model.name
    && fieldIsValid model.emailAddress
    && fieldIsValid model.password
    && Bio.isComplete model.bio



-- UPDATE


type Action
  = UpdateNameInput F.Action
  | UpdateEmailAddressInput F.Action
  | UpdatePasswordInput F.Action
  | UpdateBioForm Bio.Action


update : Action -> Model -> Model
update action model =
  case action of
    UpdateNameInput a ->
      { model | name = F.update a model.name }

    UpdateEmailAddressInput a ->
      { model | emailAddress = F.update a model.emailAddress }

    UpdatePasswordInput a ->
      { model | password = F.update a model.password }

    UpdateBioForm bioAction ->
      { model | bio = Bio.update bioAction model.bio }



-- VIEW


view : Signal.Address Action -> Model -> Html
view dispatcher model =
  let
    contramapWith =
      Signal.forwardTo dispatcher
  in
    div
      [ class "row" ]
      [ div
          [ class "row" ]
          [ F.view (contramapWith UpdateNameInput) model.name
          , F.view (contramapWith UpdateEmailAddressInput) model.emailAddress
          ]
      , div [ class "row" ] []
      , div [ class "row" ] []
      , div
          [ class "row" ]
          (F.bigValidatedFieldView (contramapWith UpdatePasswordInput) model.password)
      , div [ class "row" ] []
      , div [ class "row" ] []
      , Bio.view (contramapWith UpdateBioForm) model.bio
      ]
