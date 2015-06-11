module Particles
open Geometry
open Physics
open System

type Particle = {
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

let jitterDegree (random: Random) = 
    let range = 10.0<degree>
    let jitter = (random.NextDouble() * range) - (range/2.0)
    jitter

//TODO add jitter
let updateParticles (particles: Particle list) (elapsed: float<s>) (shipPos: Point2d) (shipThrust: Acceleration) (shipHeading: float<degree>)= 
    let random = new System.Random()
    match shipThrust with 
    | Positive accelVector-> 
        let jitterValue = jitterDegree random
        let newParticle jitter = 
            let heading = constrainDegreeTo360 <| shipHeading + 180.0<degree> + jitter
            let particleVelocity = multiplyVector accelVector 4.0 |> rotate heading
            {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0}
        let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
        newParticle 0.0<degree> :: newParticle jitterValue :: newParticle -jitterValue :: aliveParticles
        |> List.map updateParticle
    | _ -> 
        let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
        aliveParticles
        |> List.map updatePosition
