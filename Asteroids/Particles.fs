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
let randomInstance = new System.Random()

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

    let jitterDegree (random: Random) (range: float<degree>) = 
        let jitter = (random.NextDouble() * range) - (range/2.0)
        jitter

    let thrustOnlyParticles' = 
        match shipThrust with 
        | Positive _-> 
            let jitterValue0 = jitterDegree randomInstance 2.0<degree>
            let jitterValue1 = jitterDegree randomInstance 15.0<degree>
            let jitterValue2 = jitterDegree randomInstance 15.0<degree>
            let newParticle jitter = 
                let heading = constrainDegreeTo360 <| shipHeading + 180.0<degree> + jitter
                let particleVelocity = {Dx = 0.0; Dy = 0.008} |> rotateVector heading
                {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0}
            newParticle jitterValue0 :: newParticle jitterValue1 :: newParticle -jitterValue2 :: particles
        | _ -> 
            particles

    decayAndUpdateParticles elapsed aspectRatio thrustOnlyParticles'
