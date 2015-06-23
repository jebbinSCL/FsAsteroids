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
let updatePosition (aspectRatio : float) particle = 
    {particle with Position = Entities.updatePosition aspectRatio particle.Position particle.Velocity}

let updateParticle (aspectRatio : float) particle = 
    let p = updatePosition aspectRatio particle
    {p with Alpha = 1.0 - p.Age / lifeSpan} 

let decayAndUpdateParticles (elapsed: float<s>) (aspectRatio : float) particles = 
    let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
    aliveParticles
    |> List.map (updateParticle aspectRatio)

let updateParticles (particles: Particle list) (elapsed: float<s>) (aspectRatio : float) (shipPos: Point2d) (shipThrust: Acceleration) (shipHeading: float<degree>)= 
    let random = new System.Random()

    let jitterDegree (random: Random) = 
        let range = 10.0<degree>
        let jitter = (random.NextDouble() * range) - (range/2.0)
        jitter

    let particles' = 
        match shipThrust with 
        | Positive accelVector-> 
            let jitterValue = jitterDegree random
            let newParticle jitter = 
                let heading = constrainDegreeTo360 <| shipHeading + 180.0<degree> + jitter
                let particleVelocity = multiplyVector accelVector 4.0 |> rotate heading
                {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0}
            newParticle 0.0<degree> :: newParticle jitterValue :: newParticle -jitterValue :: particles
        | _ -> 
            particles
    decayAndUpdateParticles elapsed aspectRatio particles'
