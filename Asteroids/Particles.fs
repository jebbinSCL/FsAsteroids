module Particles
open Geometry
open Physics

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

let updateParticle (aspectRatio : float) (lifespan: float<s>) particle = 
    let p = updatePosition aspectRatio particle
    {p with Alpha = 1.0 - p.Age / lifespan} 

let decayAndUpdateParticles (elapsed: float<s>) (aspectRatio : float) (lifespan : float<s>) particles = 
    let aliveParticles = particles |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
    aliveParticles 
    |> List.map (updateParticle aspectRatio lifespan)

let updateParticles (particles: Particle list) (elapsed: float<s>) (aspectRatio : float) (shipPos: Point2d) (shipThrust: Acceleration) (shipHeading: float<degree>)= 

    let jitterDegreeRnd = randomDegreeAroundZero randomInstance 
    //let jitterLifeSpan() = randomInstance.NextDouble() * 0.6<s> - (0.3<s>)
    let thrustOnlyParticles' = 
        match shipThrust with 
        | Positive _-> 
            let j1,j2,j3 = jitterDegreeRnd 2.0<degree>, jitterDegreeRnd 15.0<degree>, jitterDegreeRnd 15.0<degree>
            let newParticle jitter = 
                let heading = constrainDegreeTo360 <| shipHeading + 180.0<degree> + jitter
                let particleVelocity = {Dx = 0.0; Dy = 0.008} |> rotateVector heading
                {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0}
            newParticle j1 :: newParticle j2 :: newParticle j3 :: particles
        | _ -> 
            particles

    decayAndUpdateParticles elapsed aspectRatio lifeSpan thrustOnlyParticles'
