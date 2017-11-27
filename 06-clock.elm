import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = 
  { isPaused: Bool
  , time: Time 
  } 


init : (Model, Cmd Msg)
init =
  ( Model False 0
  , Cmd.none
  )


-- UPDATE


type Msg
  = Tick Time
  | Pause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)

    Pause ->
     ({ model | isPaused = not model.isPaused}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.isPaused then
    Sub.none
  else 
    Time.every second Tick

-- VIEW

hands : Float -> Float -> (String, String)
hands angle len =
  let
    handX =
      toString (50 + len * cos angle)

    handY =
      toString (50 + len * sin angle)
  in
    (handX, handY)

view : Model -> Html Msg
view model =
  let
    ninetyDegrees = pi / 2
    secondAngle = 
      turns (Time.inMinutes model.time)

    minuteAngle =
      turns (Time.inHours model.time) - ninetyDegrees

    hourAngle = 
      turns ((Time.inHours model.time) / 12)

    (secondHandX, secondHandY) = hands secondAngle 40

    (minuteHandX, minuteHandY) = hands minuteAngle 30

    (hourHandX, hourHandY) = hands hourAngle 20
  in
    div [] 
      [ svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#DAEDE2" ] []
        , line [ x1 "50", y1 "50", x2 secondHandX, y2 secondHandY, stroke "#EA2E49" ] []
        , line [ x1 "50", y1 "50", x2 minuteHandX, y2 minuteHandY, stroke "#333745" ] []
        , line [ x1 "50", y1 "50", x2 hourHandX, y2 hourHandY, stroke "#333745" ] []
        ]
      , br [] []
      , button [onClick Pause] [Html.text "Pause"]
    ]