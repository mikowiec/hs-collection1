
module Types where


type Point = (Float,Float,Float)
type Vector = (Float,Float,Float)
type Color = (Float,Float,Float,Float)
type Quat = (Vector, Float)
type Angle = Float

type Age = Float

type Time = Float
type Direction = Point
type Radius = Float
type Size = Float


data Surface
    = Flat Color
    | Tex String
    | TexBlend Float String String
 deriving (Read, Show, Eq, Ord)


