module Entities
open Geometry

let updatePosition (position : Point2d) (velocity: Vector2d) = 
    let constrain value = 
        let upperBoundary = 2.07
        let lowerBoundary = -upperBoundary
        //TODO Switch to active pattern
        match value with 
        | tooLarge when value > upperBoundary -> lowerBoundary
        | tooSmall when value < lowerBoundary -> upperBoundary
        | _ -> value
    {X =  constrain <| position.X + velocity.Dx; Y = constrain <| position.Y + velocity.Dy}
    

