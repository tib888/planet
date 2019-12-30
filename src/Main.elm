module Main exposing (..)

import Task
import Time
import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (..)
import Planets exposing (..)

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix  
  , masses : List Planet
  , zoom : Float
  , planetZoom : Float
  , idx : Int
  , count : Int
  , speed : Int
  , mass : String
  , vx : String
  , vy : String
  , vz : String
  , px : String
  , py : String
  , pz : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 
      Time.utc 
      (Time.millisToPosix 0)       
      (List.map Planets.start masses) 
      1e-9 
      1 
      0 
      0 
      6000
      "1E30"
      "0" "-13.6e3" "0"
      "500e9" "0" "0"
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )

-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | PlanetZoomIn
  | PlanetZoomOut
  | ZoomIn
  | ZoomOut
  | PlanetPlus
  | PlanetMinus
  | SpeedPlus
  | SpeedMinus
  | Mass String
  | SpeedX String
  | SpeedY String
  | SpeedZ String
  | PosX String
  | PosY String
  | PosZ String
  | FillCommet
  | StartCommet

repeatedApply: Int -> a -> (a -> a) -> a
repeatedApply n data func =
  if n < 1 then
    data
  else 
    repeatedApply (n - 1) (func data) func

toFloatx : String -> Float
toFloatx t =
  Maybe.withDefault 0 (String.toFloat t)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Tick newTime ->
        ( { model | count = model.count + 1, time = newTime, masses = updatePlanets True 60 (repeatedApply model.speed model.masses (updatePlanets False 15)) } 
        , Cmd.none
        )

      AdjustTimeZone newZone ->
        ( { model | zone = newZone }
        , Cmd.none
        )

      PlanetZoomIn ->
        ( { model | planetZoom = model.planetZoom * 2 }
        , Cmd.none
        )

      PlanetZoomOut ->
        ( { model | planetZoom = model.planetZoom / 2 }
        , Cmd.none
        )

      ZoomIn ->
        ( { model | zoom = model.zoom * 1.41 }
        , Cmd.none
        )

      ZoomOut ->
        ( { model | zoom = model.zoom / 1.41 }
        , Cmd.none
        )

      PlanetPlus ->
        ( { model | idx = model.idx + 1 }
        , Cmd.none
        )

      PlanetMinus ->
        ( { model | idx = model.idx - 1 }
        , Cmd.none
        )

      SpeedPlus ->
        ( { model | speed = model.speed * 2 }
        , Cmd.none
        )

      SpeedMinus ->
        ( { model | speed = 
            if model.speed > 1 then 
              model.speed // 2 
            else 
              1 }
        , Cmd.none
        )
      
      Mass m  ->
        ( { model | mass = m }, Cmd.none )
      SpeedX v ->
        ( { model | vx = v }, Cmd.none )
      SpeedY v ->
        ( { model | vy = v }, Cmd.none )
      SpeedZ v ->
        ( { model | vz = v }, Cmd.none )
      PosX p ->
        ( { model | px = p }, Cmd.none )
      PosY p ->
        ( { model | py = p }, Cmd.none )
      PosZ p ->
        ( { model | pz = p }, Cmd.none )

      StartCommet -> 
        let 
          commet = 
            { name = "Commet"
            , color = "Black"
            , mass = toFloatx model.mass
            , d = 6000000
            , v = Vector (toFloatx model.vx) (toFloatx model.vy) (toFloatx model.vz)
            , pos = Vector (toFloatx model.px) (toFloatx model.py) (toFloatx model.pz)
            , history = []
            }
        in 
          ( { model | masses = commet :: model.masses } , Cmd.none )
      
      FillCommet -> 
        let 
          m = case List.drop model.idx model.masses of
            (b :: _) -> { model | mass = String.fromFloat b.mass
                        , vx = String.fromFloat b.v.x, vy = String.fromFloat b.v.y, vz = String.fromFloat b.v.z
                        , px = String.fromFloat b.pos.x, py = String.fromFloat b.pos.y, pz = String.fromFloat b.pos.z 
                        }
            _ -> model
        in 
          ( m, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 100 Tick

-- VIEW

viewPath1 : Float -> Float -> Float -> String -> List Vector -> List (Svg msg)
viewPath1 scale ccx ccy color history =
    case history of 
      (h1 :: (h2 :: ta)) ->
        line
          [ x1 (String.fromFloat (ccx + h1.x * scale))
          , y1 (String.fromFloat (ccy + h1.y * scale))
          , x2 (String.fromFloat (ccx + h2.x * scale))
          , y2 (String.fromFloat (ccy + h2.y * scale))
          , stroke color
          , strokeWidth "1"
          ]
          [] :: viewPath1 scale ccx ccy color (h2::ta)
      _ -> []

viewPath2 : Float -> Float -> Float ->  Float -> String -> List Vector -> List (Svg msg)
viewPath2 scale ccx ccy rad color history =
    case history of 
      (h1 :: ta) ->
        circle
          [ cx (String.fromFloat (ccx + h1.x * scale))
          , cy (String.fromFloat (ccy + h1.y * scale))
          , r (String.fromFloat rad)
          , fill color
          ]
          [] :: viewPath2 scale ccx ccy rad color (List.drop (24*7) ta)
      _ -> []
  
viewPlanet : Float -> Float -> Float -> Float -> Planet -> List (Svg msg)
viewPlanet scale bolygoscale ccx ccy planet =
  let
    radius = planet.d * scale * bolygoscale
    x = ccx + planet.pos.x * scale
    y = ccy + planet.pos.y * scale
  in
    circle
      [ cx (String.fromFloat x)
      , cy (String.fromFloat y)
      , r (String.fromFloat radius)
      , fill planet.color
      ]
      [] 
      -- :: viewPath2 scale ccx ccy 1 planet.color planet.history
      :: viewPath1 scale ccx ccy planet.color planet.history

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  div [] [Html.text p, input [ Html.Attributes.type_ t, placeholder p, value v, onInput toMsg ] []]

view : Model -> Html Msg
view model =
  let 
    center = 
      case List.drop model.idx model.masses of
        (b :: _) -> b.pos
        _ -> zero

  --   hour   = toFloat (Time.toHour   model.zone model.time)
  --   minute = toFloat (Time.toMinute model.zone model.time)
  --   second = toFloat (Time.toSecond model.zone model.time)
  in
  div []
    [ (viewInput "text" "Mass " model.mass Mass)
    , (viewInput "text" "Speed X " model.vx SpeedX)      
    , (viewInput "text" "Speed Y " model.vy SpeedY)
    , (viewInput "text" "Speed Z " model.vz SpeedZ)
    , (viewInput "text" "Pos X " model.px PosX)
    , (viewInput "text" "Pos Y " model.py PosY)
    , (viewInput "text" "Pos Z " model.pz PosZ)
    , button [ onClick FillCommet ] [ Html.text "FillCommet commet with center!" ]
    , button [ onClick StartCommet ] [ Html.text "Start commet!" ]
    , div [] []
    , button [ onClick ZoomOut ] [ Html.text "scale -" ]
    , button [ onClick ZoomIn ] [ Html.text "scale +" ]
    , button [ onClick PlanetZoomOut ] [ Html.text "planet scale -" ]
    , button [ onClick PlanetZoomIn ] [ Html.text "planet scale +" ]
    , button [ onClick PlanetMinus ] [ Html.text "center -" ]    
    , button [ onClick PlanetPlus ] [ Html.text "center +" ]
    , button [ onClick SpeedMinus ] [ Html.text "speed -" ]
    , button [ onClick SpeedPlus ] [ Html.text "speed +" ]
    , svg
      [ viewBox "0 0 1024 600"
      , Svg.Attributes.width "1024"
      , Svg.Attributes.height "600"
      ]
      (List.concat (List.map (viewPlanet model.zoom model.planetZoom (512 - center.x * model.zoom) (300 - center.y * model.zoom)) model.masses))
    ]