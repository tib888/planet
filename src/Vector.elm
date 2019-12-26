module Vector exposing (..)

type alias Vector = 
  { x : Float
  , y : Float
  , z : Float
  }

sub : Vector -> Vector -> Vector
sub l r =
  { x = l.x - r.x
  , y = l.y - r.y
  , z = l.z - r.z 
  }

add : Vector -> Vector -> Vector
add l r =
  { x = l.x + r.x
  , y = l.y + r.y
  , z = l.z + r.z 
  }

mul : Vector -> Float -> Vector
mul l r =
  { x = l.x * r
  , y = l.y * r
  , z = l.z * r
  }

divide : Vector -> Float -> Vector
divide l r =
  { x = l.x / r
  , y = l.y / r
  , z = l.z / r
  }

scalarMul : Vector -> Vector -> Float
scalarMul l r =
  l.x * r.x + l.y * r.y + l.z * r.z 

len2 : Vector -> Float
len2 v =
  scalarMul v v 

len : Vector -> Float
len v =
  sqrt (len2 v)   

zero : Vector
zero = Vector 0 0 0
  