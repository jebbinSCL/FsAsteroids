module Entities
open Geometry  

let neutralRatio = 1.0
let positionMaximumValue aspectRatio= 
    let allowedExtra = 0.08
    (2.0 * aspectRatio) + allowedExtra

let updatePosition (aspectRatio : float) (position : Point2d) (velocity: Vector2d) = 
    let constrain ratio value = 
        let upperBoundary = positionMaximumValue ratio
        let lowerBoundary = -upperBoundary
        match value with 
        | tooLarge when value > upperBoundary -> lowerBoundary
        | tooSmall when value < lowerBoundary -> upperBoundary
        | _ -> value
    {X =  constrain aspectRatio <| position.X + velocity.Dx; Y = constrain neutralRatio <| position.Y + velocity.Dy}
    

