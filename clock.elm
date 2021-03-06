import Html exposing (Html)
import Html.App as Html
import Html.Attributes as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing (Time, second)
import Dict as Dict
import String as String
import Json.Encode

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type ShapeState =
  Rest
  | Active

type alias Shape =
  ShapeState -> Int -> Svg Msg

type alias Task =
  { shape : Shape
  , seconds : Int
  , label : String
  , music : String
  }

type alias Model =
  { time : Time
  , secondsLeft : Int
  , taskIndex : Int
  , task : Maybe Task
  , tasks : List (Maybe Task)
  }

init : (Model, Cmd Msg)
init =
  ( { time = 0
    , secondsLeft = 0
    , taskIndex = -1
    , task = Nothing
    , tasks =
      [ Just { shape = dummy, seconds = 5*60, label = "wekken", music = "wekken.ogg" }
      , Just { shape = dummy, seconds = 5*60, label = "opstaan", music = "opstaan.mp3" }
      , Just { shape = dummy, seconds = 10*60, label = "wc", music = "wc.ogg" }

      , Just { shape = dummy, seconds = 5*60, label = "wassen", music = "wassen.mp3" }
      , Just { shape = shirt, seconds = 5*60, label = "aankleden", music = "aankleden.ogg" }
      , Just { shape = dummy, seconds = 4*60, label = "tandenpoetsen", music = "tandenpoetsen.mp3" }

      , Just { shape = dummy, seconds = 4*60, label = "tandenpoetsen", music = "tandenpoetsen2.mp3" }
      , Nothing
      , Nothing
      ]
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time
  | StartTask Int
  | EndTask

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time' ->
      let
        model' = case model.task of
          Nothing -> { model | time = time' }
          Just task ->
            case model.secondsLeft of
              0 -> { model | time = time', task = Nothing, taskIndex = -1 }
              secondsLeft ->
                { model | time = time', secondsLeft = secondsLeft - 1}
      in
        ( model', Cmd.none )
    StartTask idx ->
      case model.task of
        Nothing ->
          let
            model' = case Dict.get idx (Dict.fromList (List.indexedMap (,) model.tasks)) of
              Nothing -> { model | task = Nothing, taskIndex = -1 }
              Just Nothing -> { model | task = Nothing, taskIndex = -1 }
              Just (Just task) ->
                { model | task = Just task, taskIndex = idx, secondsLeft = task.seconds }
          in
            ( model', Cmd.none )
        _ -> update EndTask model
    EndTask ->
      ( { model | task = Nothing, taskIndex = -1 }, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW

dummy : Shape
dummy state idx =
  let
    px = 100 * (idx % 3)
    py = 100 * (idx // 3)
    (color, strokeColor)  = case state of
      Rest -> ("green", "lightgreen")
      Active -> ("red", "darkred")
  in
    g [ transform ("translate(" ++ (toString px) ++ "," ++ (toString py) ++ ")") ]
    [ rect
      [ stroke strokeColor
      , fill color
      , x "20"
      , y "20"
      , width "60"
      , height "60"
      , onClick (StartTask idx)
      ] []
    ]

shirt : Shape
shirt state idx =
  let
    x = 10 + 100 * (idx % 3)
    y = 20 + 100 * (idx // 3)
    (color, strokeColor)  = case state of
      Rest -> ("green", "lightgreen")
      Active -> ("red", "darkred")
  in
    g [ transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")") ]
    [ polygon
      [ stroke strokeColor
      , fill color
      , points "20,0 0,10 10,30 20,20 20,60 60,60 60,20 70,30 80,10 60,0 50,0 45,5 35,5 30,0"
      , onClick (StartTask idx)
      ] []
    ]

taskView : Int -> Int -> Maybe Task -> Svg Msg
taskView index activeIndex maybeTask =
  let
    state = case activeIndex == index of
      True -> Active
      False -> Rest
    shape = case maybeTask of
      Nothing -> rect [] []
      Just task -> task.shape state index
  in
    shape


taskGrid : Model -> List (Svg Msg)
taskGrid model =
  let
    label = case model.task of
      Nothing -> "klok"
      Just task ->
        task.label ++ " "
          ++ (toString (model.secondsLeft // 60))
          ++ "'"
          ++ (toString (model.secondsLeft % 60))
          ++ "\""
  in
    [ rect [ width "300", height "300" ] []
    , line [ x1 "0", y1 "100", x2 "300", y2 "100" ] []
    , line [ x1 "0", y1 "200", x2 "300", y2 "200" ] []
    , line [ x1 "100", y1 "0", x2 "100", y2 "300" ] []
    , line [ x1 "200", y1 "0", x2 "200", y2 "300" ] []
    ]
    ++ List.map
      (\(idx, task) -> taskView idx model.taskIndex task)
      (List.indexedMap (,) model.tasks)
    ++ [ text' [ transform "translate(0,340)", fill "white", stroke "none", fontSize "200%" ] [ text label ]
    ]

normalClock : Time -> List (Svg Msg)
normalClock time =
  let
    angleSeconds =
      6 * ((floor (Time.inSeconds time)) % 60) |> toString
    angleMinutes =
      6 * ((floor (Time.inMinutes time)) % 60) |> toString
    angleHours =
      30 * ((2 + (floor (Time.inHours time))) % 12) |> toString
  in
    [ line [ transform ("rotate(" ++ angleSeconds ++ ")"), x2 "0", y2 "-0.9" ] []
    , line [ transform ("rotate(" ++ angleMinutes ++ ")"), x2 "0", y2 "-0.8", strokeWidth "0.02"] []
    , line [ transform ("rotate(" ++ angleHours ++ ")"), x2 "0", y2 "-0.5", strokeWidth "0.03"] []
    ]

polarToCartesian centerX centerY radius angleInDegrees =
  let
    angleInRadians = degrees angleInDegrees
    (x,y) = fromPolar (radius, angleInRadians)
  in
    (centerX + x, centerY + y)

arc x y radius startAngle endAngle =
  let
    (startx, starty) = polarToCartesian x y radius endAngle
    (endx, endy) = polarToCartesian x y radius startAngle
    largeArcFlag = case endAngle - startAngle <= 180 of
      True -> "0"
      False -> "1"
  in
    Svg.path
      [ d (String.join " "
        [ "M", toString startx, toString starty
        , "A", toString radius, toString radius, "0", largeArcFlag, "0", toString endx, toString endy
        ])
      , stroke "green"
      , fill "none"
      , strokeWidth "0.4"
      ]
      []

taskClock : Int -> Int -> List (Svg Msg)
taskClock secondsLeft secondsTotal =
  let
    angle = 360 * ((toFloat secondsLeft) / (toFloat secondsTotal))
  in
    [ arc 0 0 0.7 0 angle ]

view : Model -> Html Msg
view model =
  let
    clock = case model.task of
      Nothing ->
        normalClock model.time
      Just task ->
        taskClock model.secondsLeft task.seconds
    music = case model.task of
      Nothing -> []
      Just task ->
        [ foreignObject
          [ width "1", height "1" ]
          [ Html.audio
            [ Html.src <| "music/" ++ task.music
            , Html.autoplay True
            , Html.loop True
            ]
            []
          ]
        ]
  in
    svg [ viewBox "0 0 800 480" ]
      (
        [ rect [ width "100%", height "100%", fill "black"] []
        , g [ transform "translate(200,200) scale(190)", strokeWidth "0.01", stroke "white" ]
          ( [ circle [ r "1" ] [] ]
          ++ clock
          )
        , g [ transform "translate(450,50)", stroke "white" ] (taskGrid model)
        ]
        ++ music
      )
