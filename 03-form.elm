import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (length)
import Regex exposing (regex, contains)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , isSubmiting: Bool
  }


model : Model
model =
  Model "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | 
        password = password, 
        isSubmiting = False
      }

    PasswordAgain password ->
      { model | 
        passwordAgain = password,
        isSubmiting = False
      }

    Submit ->
      { model | isSubmiting = True}

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [type_ "submit", onClick Submit] [text "Submit"]
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let (color, message) =
    validatePasswords model.password model.passwordAgain
  in
    if model.isSubmiting then
      div [ style [("color", color)] ] [ text message ]
    else 
      div [] []


validatePasswords : String -> String -> (String, String)
validatePasswords pass1 pass2 =
  if (length pass1 < 8) || (length pass2 < 8) then
    ("red", "Should be at least 8 char")
  else if (not (contains passRegex pass1)) || (not (contains passRegex pass2)) then
    ("red", "Should contain upper lower and number")
  else if pass1 /= pass2 then
    ("red", "Passwords do not match!")
  else
    ("green", "OK") 

passRegex = (regex "^(?=.*\\d)(?=.*[a-z])(?=.*[A-Z])(?!.*\\s).*$")
