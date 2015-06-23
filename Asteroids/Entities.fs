module Entities
open Geometry  

let updatePosition (aspectRatio : float) (position : Point2d) (velocity: Vector2d) = 
    let constrain ratio value = 
        let allowedExtra = 0.08
        let upperBoundary = (2.0 * ratio) + allowedExtra
        let lowerBoundary = -upperBoundary
        match value with 
        | tooLarge when value > upperBoundary -> lowerBoundary
        | tooSmall when value < lowerBoundary -> upperBoundary
        | _ -> value
    let neutralRatio = 1.0
    {X =  constrain aspectRatio <| position.X + velocity.Dx; Y = constrain neutralRatio <| position.Y + velocity.Dy}
    

