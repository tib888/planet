module Main exposing (..)

import Browser
import Task
import Time
import Html exposing (Html, button, div, text)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Vector exposing (..)
import Planet exposing (..)

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
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) (List.map start masses) 1e-9 1 0 0
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    store = 0 == modBy 100 model.count
  in 
    case msg of
      Tick newTime ->
        ( { model | count = model.count + 1, time = newTime, masses = updatePlanets store 3600 model.masses } 
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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 16 Tick

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
    [ button [ onClick ZoomOut ] [ Html.text "s-" ]
    , button [ onClick ZoomIn ] [ Html.text "s+" ]
    , button [ onClick PlanetZoomOut ] [ Html.text "b-" ]
    , button [ onClick PlanetZoomIn ] [ Html.text "b+" ]
    , button [ onClick PlanetMinus ] [ Html.text "c-" ]
    , button [ onClick PlanetPlus ] [ Html.text "c+" ]
    , svg
      [ viewBox "0 0 1024 600"
      , width "1024"
      , height "600"
      ]
      (List.concat (List.map (viewPlanet model.zoom model.planetZoom (512 - center.x * model.zoom) (300 - center.y * model.zoom)) model.masses))
    ]