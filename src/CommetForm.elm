module CommetForm exposing (Model, Msg, init, update, view, toMass, fromMass)

import Html exposing (Html, div, text, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (type_, placeholder, value)
import Planets exposing (Planet)
import Vector exposing (..)

type alias Model =
  { mass : String
  , vx : String
  , vy : String
  , vz : String
  , px : String
  , py : String
  , pz : String
  }

init : Model
init = Model 
  ""
  "" "" ""
  "" "" ""
  
type Msg
  = Mass String
  | SpeedX String
  | SpeedY String
  | SpeedZ String
  | PosX String
  | PosY String
  | PosZ String

update : Msg -> Model -> Model
update msg model =
    case msg of
      Mass m  ->
        { model | mass = m }
      SpeedX v ->
        { model | vx = v }
      SpeedY v ->
        { model | vy = v }
      SpeedZ v ->
        { model | vz = v }
      PosX p ->
        { model | px = p }
      PosY p ->
        { model | py = p }
      PosZ p ->
        { model | pz = p }

toFloatx : String -> Float
toFloatx t =
  Maybe.withDefault 0 (String.toFloat t)

toMass: Model -> Planet
toMass model = 
  { name = "Commet"
  , color = "Black"
  , mass = toFloatx model.mass
  , d = 6000000
  , v = Vector (toFloatx model.vx) (toFloatx model.vy) (toFloatx model.vz)
  , pos = Vector (toFloatx model.px) (toFloatx model.py) (toFloatx model.pz)
  , history = []
  }

fromMass: Planet -> Model
fromMass b =
  { mass = String.fromFloat b.mass
  , vx = String.fromFloat b.v.x 
  , vy = String.fromFloat b.v.y
  , vz = String.fromFloat b.v.z
  , px = String.fromFloat b.pos.x 
  , py = String.fromFloat b.pos.y 
  , pz = String.fromFloat b.pos.z
  }

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  div [] [Html.text p, input [ Html.Attributes.type_ t, placeholder p, value v, onInput toMsg ] []]


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Mass " model.mass Mass
    , viewInput "text" "Speed X " model.vx SpeedX      
    , viewInput "text" "Speed Y " model.vy SpeedY
    , viewInput "text" "Speed Z " model.vz SpeedZ
    , viewInput "text" "Pos X " model.px PosX
    , viewInput "text" "Pos Y " model.py PosY
    , viewInput "text" "Pos Z " model.pz PosZ
    ]