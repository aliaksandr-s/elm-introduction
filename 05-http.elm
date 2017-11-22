import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , errMessage: String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic loadingGif ""
  , getRandomGif topic
  )

loadingGif : String
loadingGif = "./static/loading.gif"

topics : List String
topics =  [ "cat", "dog", "girl", "food", "fun", "lol" ]

-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | SetTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      ({ model | gifUrl = loadingGif }, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      (Model model.topic newUrl "", Cmd.none)

    NewGif (Err message) ->
      ({ model | errMessage = toString message }, Cmd.none)

    SetTopic newTopic ->
      (Model newTopic loadingGif "", getRandomGif model.topic)

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , select [ onInput SetTopic ] (topics |> List.map topicToOption)
    , br [] []
    , br [] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , div [] [showImg model]
    ]

topicToOption : String -> Html Msg
topicToOption topic =
  option [ value topic ] [ text topic ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP
showImg : Model -> Html Msg
showImg model =
  if model.errMessage == "" then
    img [src model.gifUrl] []
  else 
    div [ style [("color", "red"), ("marginTop", "10px")] ] [text model.errMessage]

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
