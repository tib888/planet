module Planets exposing (..)
import Vector exposing (..)

type PlanetInit = 
    PlanetInit
    { name : String
    , color : String
    , mass : Float
    , d : Float
    , dist : Float
    , v : Float
    , year : Float
    , owner : Maybe PlanetInit 
    }

sun: PlanetInit
sun = PlanetInit
  { name = "Sun"
  , color = "tan"
  , mass = 330000.0
  , d = 10000         -- 1392000 
  , dist = 0
  , v = 0 
  , year = 0
  , owner = Nothing
  }

merkur: PlanetInit
merkur = PlanetInit
  { name = "Merkur"
  , color = "gray"
  , mass = 0.055  -- 5.974e24 kg
  , d = 4878  -- km
  , dist = 57.9  -- 1e6 km
  , v = 47.6  -- km/s 
  , year = 58.65 -- year period in earth days
  , owner = Just sun
  }

venus: PlanetInit
venus = PlanetInit
  { name = "Venus"
  , color = "orange"
  , mass = 0.815  -- 5.974e24 kg
  , d = 12104  -- km
  , dist = 108.2  -- 1e6 km
  , v = 35.04  -- km/s 
  , year = 224.7 -- year period in earth days
  , owner = Just sun
  }

earth: PlanetInit
earth = PlanetInit
  { name = "Earth"
  , color = "blue"
  , mass = 1.0  -- 5.974e24 kg
  , d = 12756  -- km
  , dist = 149.6  -- 1e6 km
  , v = 29.765  -- km/s 
  , year = 365.26 -- year period in earth days
  , owner = Just sun
  }

moon: PlanetInit
moon = PlanetInit
  { name = "Hold"
  , color = "gray"
  , mass = 0.0123  -- 5.974e24 kg
  , d = 3476  -- km
  , dist = 0.384404  -- 1e6 km
  , v = 1.025  -- km/s 
  , year = 27.21222 -- ?
  , owner = Just earth
  }

mars: PlanetInit
mars = PlanetInit
  { name = "Mars"
  , color = "orange"
  , mass = 0.107
  , d = 6794
  , dist = 227.9
  , v = 24.14
  , year = 687
  , owner = Just sun
  }

jupiter: PlanetInit
jupiter = PlanetInit
  { name = "Jupiter"
  , color = "red"
  , mass = 317.9
  , d = 142800
  , dist = 778.3
  , v = 13.06
  , year = 11.863 * 365.26
  , owner = Just sun
  }

szaturnus: PlanetInit
szaturnus = PlanetInit
  { name = "Szaturnus"
  , color = "gold"
  , mass = 95.16
  , d = 120540
  , dist = 1429
  , v = 9.65
  , year = 29.42 * 365.26
  , owner = Just sun
  }

uranus: PlanetInit
uranus = PlanetInit
  { name = "Uranus"
  , color = "lightblue"
  , mass = 14.54
  , d = 51118
  , dist = 2875
  , v = 6.83
  , year = 83.75 * 365.26
  , owner = Just sun
  }

neptunus: PlanetInit
neptunus = PlanetInit
  { name = "Neptunus"
  , color = "blue"
  , mass = 17.2
  , d = 48600
  , dist = 4504
  , v = 5.43
  , year = 163.73 * 365.26
  , owner = Just sun
  }

masses : List PlanetInit
masses = 
  [ sun
  , merkur
  , venus
  , earth 
  , moon
  , mars
  , jupiter
  , szaturnus
  , uranus
  , neptunus
  ]

type alias Planet = 
  { name : String
  , color : String
  , mass : Float
  , d : Float
  , v : Vector
  , pos : Vector
  , history : List Vector
  }

start : PlanetInit -> Planet
start b = 
  let 
    (p0, v0) = case b of
      PlanetInit planet -> 
        case planet.owner of
          Just o -> ((start o).pos, (start o).v) 
          _ -> (zero, zero)

  --   vv = b.dist * 2e9 * 3.141592653589795 / (b.year * 24 * 3600)
  in
    case b of
      PlanetInit planet ->
        { name = planet.name
        , color = planet.color
        , mass = planet.mass * 5.974e24
        , d = planet.d * 1e3
        , v = add v0 (Vector 0 (planet.v * 1e3) 0)  
        , pos = add p0 (Vector (planet.dist * 1e9) 0 0)
        , history = []
        }

gravityForce : Planet -> Planet -> Vector
gravityForce b1 b2 =
  let
    rv = sub b2.pos b1.pos
    r2 = len2 rv
    rd = mul rv (1 / sqrt r2)
    f = 6.67259e-11 * b1.mass * b2.mass / r2
  in
    if r2 < 1 then 
      zero
    else
      mul rd f

updatePlanet : Bool -> Float -> List Planet -> Planet -> Planet
updatePlanet store dt blist b =
  let
    forces = List.map (gravityForce b) blist
    f = List.foldr add zero forces
    a = mul f (1.0 / b.mass)
    newpos = add b.pos (mul b.v dt)
    newv = add b.v (mul a dt)
  in
    { b | pos = newpos, v = newv, history = if store then b.pos::b.history else b.history }
  
updatePlanets : Bool -> Float -> List Planet -> List Planet
updatePlanets store dt blist = 
  List.map (updatePlanet store dt blist) blist