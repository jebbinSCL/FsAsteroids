module TrailParticles
open Geometry
open Physics
open Particles

let lifeSpan = 3.0<s>

let updateParticles (particles: Particle list) (elapsed: float<s>) (aspectRatio : float) (shipPos: Point2d) (shipThrust: Acceleration) (shipHeading: float<degree>)=
    let particles' = 
        match shipThrust with 
        | Positive _-> 
            let particleVelocity = {Dx=0.0; Dy = 0.0}
            {Position=shipPos; Velocity=particleVelocity; Age=0.0<s>; Alpha = 1.0} :: particles
        | _ -> 
            particles
    decayAndUpdateParticles elapsed aspectRatio particles'
