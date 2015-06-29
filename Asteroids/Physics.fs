module Physics

open Geometry

[<Measure>] type s

type Acceleration = 
    | Positive of Vector2d
    | Negative of Vector2d
    | Neutral 

let neutralAcceleration = Neutral
let neutralPosition = {X = 0.0; Y = 0.0;}
let neutralHeading = 0.0<degree>
let neutralRotationalVelocity = 0.0<degree>
let neutralVelocity = {Dx = 0.0; Dy = 0.0} 