import Html exposing (..)
import Html.Attributes exposing (attribute, class, style, src)
import Html.Events exposing (..)
import Style exposing (..)
import Random
import Array exposing (..)
import Platform.Cmd exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace1 : Int
  , dieFace2 : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1 1
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFaces (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, generateFaces)

    NewFaces (newFace1, newFace2) ->
      (Model newFace1 newFace2, Cmd.none)


generateFaces : Cmd Msg
generateFaces =
  Random.generate NewFaces (Random.pair (Random.int 0 5) (Random.int 0 5))

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString (model.dieFace1 + 1)) ]
    , button [ onClick Roll ] [ text "Roll" ]
    , div [style [display "flex", justifyContent "space-evenly"]] [ br [] []
      , div [style (computeStyle model.dieFace1)] []
      , div [style (computeStyle model.dieFace2)] []
      ]
    ]

computeStyle randNum =
  [ background ("url(static/dice.jpg) " ++ (getImgCoord imgCoords randNum))
  , width (px 591)
  , height (px 591)
  ]

imgCoords = 
  [ "-56px -53px"
  , "-56px -925px"
  , "-928px -53px"
  , "-928px -925px"
  , "-1801px -925px"
  , "-1801px -53px"
  ]

getImgCoord coords i =
  case Array.get i (Array.fromList coords) of
    Nothing -> 
      ""

    Just coord ->
      coord