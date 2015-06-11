module Physics

open Geometry

[<Measure>] type s

type Acceleration = 
    | Positive of Vector2d
    | Negative of Vector2d
    | Neutral 