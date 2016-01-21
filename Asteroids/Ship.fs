module Ship

open Geometry
open Physics

type Ship = { 
    Position: Point2d
    BodyWrtOrigin : ColoredTriangle2d
    Heading: float<degree>
    RotationalVelocity : float<degree>
    Velocity: Vector2d 
    Thrust: Acceleration
} 

let initialShip =     
    { 
        Position = Physics.neutralPosition
        BodyWrtOrigin = 
            {
            P1 ={Color={R=0.2; G=0.9; B=1.0}; Point= {X = 0.0; Y = 0.1;}}; 
            P2 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = -0.1; Y = -0.1;}}; 
            P3 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = 0.1; Y = -0.1;}};
            }
        Heading = Physics.neutralHeading
        RotationalVelocity = Physics.neutralRotationalVelocity
        Velocity = Physics.neutralVelocity 
        Thrust = Neutral
    }

let updateThrust newThrust ship = {ship with Thrust = newThrust}

let updateRotationalVelocity newRotationalVelocity ship = {ship with RotationalVelocity = newRotationalVelocity }

let updateHeading ship = {ship with Heading = constrainDegreeTo360 <| ship.Heading + ship.RotationalVelocity }

let updateVelocity ship = 
    let vel = ship.Velocity
    let newVel = 
        match ship.Thrust with 
        | Positive acc -> 
            let constrain value = 
                let upperBoundary = 0.04
                let lowerBoundary = -upperBoundary
                max lowerBoundary value |> min upperBoundary
            let thrust = rotateVector ship.Heading acc
            {Dx = constrain <| vel.Dx + thrust.Dx; Dy = constrain  <| vel.Dy + thrust.Dy}
        | Negative dec -> 
            let shrink change value = 
                let snapBoundary = 0.01
                let targetValue = 0.0
                match value with 
                | nearZero when value > -snapBoundary && value < snapBoundary -> targetValue
                | tooLarge when value > targetValue -> value - abs change
                | tooSmall when value < targetValue -> value + abs change
                | _ -> value
            let decelAngle = convertRadianToDegree <| angleBetweenVectors vel {Dx = 0.0; Dy = 1.0}
            let decel = rotateVector decelAngle dec
            {Dx = shrink decel.Dx vel.Dx ; Dy = shrink decel.Dy vel.Dy}
        | Neutral -> vel
    {ship with Velocity = newVel}

let updatePosition (aspectRatio : float) ship = {ship with Position = Entities.updatePosition aspectRatio ship.Position ship.Velocity}
