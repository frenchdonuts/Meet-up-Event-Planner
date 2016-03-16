module Components.GuestList (..) where

-- MODEL


type alias Model =
  { guests : List String
  , addGuestInput : FI.Model
  }


init : Model
init =
  { guests = []
  , addGuestInput = FI.withLabel "Add Guest: "
  }



-- UPDATE


type Action
  = SetAddGuestInput FI.Action
  | AddGuest String


update action model =
  case action of
    SetAddGuestInput a ->
      { model | addGuestInput = FI.update a model.addGuestInput }

    AddGuest guest ->
      { model | guests = model.guests ++ [ guest ] }



-- VIEW


view dispatcher model =
  div
    []
    [ h1 [] [ text "Okay. Who's invited?" ]
    , ul [] (map (\guest -> guestView guest) model.guests)
    , FI.text_ (contramapWith SetAddGuestInput) model.addGuestInput
    ]
