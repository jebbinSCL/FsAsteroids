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

let neutralPosition = {X = 0.0; Y = 0.0;}
let neutralHeading = 0.0<degree>
let neutralRotationalVelocity = 0.0<degree>
let neutralVelocity = {Dx = 0.0; Dy = 0.0} 
let neutralThrust = Neutral

let initialShip =     
    { 
        Position = neutralPosition
        BodyWrtOrigin = 
            {
            P1 ={Color={R=0.2; G=0.9; B=1.0}; Point= {X = 0.0; Y = 0.1;}}; 
            P2 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = -0.1; Y = -0.1;}}; 
            P3 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = 0.1; Y = -0.1;}};
            }
        Heading = neutralHeading
        RotationalVelocity = neutralRotationalVelocity
        Velocity = neutralVelocity 
        Thrust = neutralThrust
    }

let updateThrust newThrust ship = {ship with Thrust = newThrust}

let updateRotationalVelocity newRotationalVelocity ship = {ship with RotationalVelocity = newRotationalVelocity }

let updateHeading ship = 
    let heading = ship.Heading
    {ship with Heading = constrainDegreeTo360 <| heading + ship.RotationalVelocity }

let updateVelocity ship = 
    let vel = ship.Velocity
    let newVel = 
        match ship.Thrust with 
        | Positive acc -> 
            let constrain value = 
                let upperBoundary = 0.05
                let lowerBoundary = -0.05
                max lowerBoundary value |> min upperBoundary
            let thrust = rotate ship.Heading acc
            {Dx = constrain <| vel.Dx + thrust.Dx; Dy = constrain  <| vel.Dy + thrust.Dy}
        | Negative dec -> 
            //TODO Switch to active pattern
            //TODO FIX: Deceleration should be in opposite direction of Velocity, not opposite to heading. 
            let shrink change value = 
                match value with 
                | nearZero when value > -0.01 && value < 0.01 -> 0.0
                | tooLarge when value > 0.0 -> value - abs change
                | tooSmall when value < 0.0 -> value + abs change
                | _ -> value
            let decelAngle = convertRadianToDegree <| angleBetweenVectors vel {Dx = 0.0; Dy = 1.0}
            let decel = rotate decelAngle dec
            {Dx = shrink decel.Dx vel.Dx ; Dy = shrink decel.Dy vel.Dy}
        | Neutral -> vel
    {ship with Velocity = newVel}

//TODO make bounds more flexible and dependant on window size / aspect
let updatePosition ship = 
    {ship with Position = Entities.updatePosition ship.Position ship.Velocity}
