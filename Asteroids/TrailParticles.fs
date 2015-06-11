module TrailParticles
open Geometry
open Physics

type TrailParticle = {
    Position: Point2d
    Velocity: Vector2d 
    Age: float<s>
    Alpha : float
}

let lifeSpan = 3.0<s>

//TODO basically the same as ship updatePosition
let updatePosition particle = 
    let constrain value = 
        let upperBoundary = 2.07
        let lowerBoundary = -upperBoundary
        //TODO Switch to active pattern
        match value with 
        | tooLarge when value > upperBoundary -> lowerBoundary
        | tooSmall when value < lowerBoundary -> upperBoundary
        | _ -> value
    let pos = particle.Position
    let vel = particle.Velocity
    let newPos = {X =  constrain <| pos.X + vel.Dx; Y = constrain <| pos.Y + vel.Dy}
    {particle with Position = newPos}

let updateParticle particle = 
    let p = updatePosition particle
    {p with Alpha = 1.0 - p.Age / lifeSpan} 

let updateParticles (particles: TrailParticle list) (elapsed: float<s>) (shipPos: Point2d) (shipThrust: Acceleration) (shipHeading: float<degree>)=
    match shipThrust with 
    | Positive accelVector-> 
        let heading = constrainDegreeTo360 <| shipHeading + 180.0<degree>
        let particleVelocity = {Dx=0.0; Dy = 0.0}
        let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
        {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0} :: aliveParticles
        |> List.map updateParticle
    | _ -> 
        //let particleVelocity = rotate 180.0<degree> accelVector
        let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
        aliveParticles
        |> List.map updatePosition
