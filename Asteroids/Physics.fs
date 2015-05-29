module Physics

open Geometry

type Acceleration = 
    | Positive of Vector2d
    | Negative of Vector2d
    | Neutral 