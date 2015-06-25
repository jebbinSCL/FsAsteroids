module Rockets

open Geometry
open Physics
open Particles

let lifeSpan = 0.7<s>

let createRocket (shipPos: Point2d) (shipHeading: float<degree>) = 
    let velocity = {Dx = 0.0; Dy = 0.075} |> rotateVector shipHeading
    {Position=shipPos; Velocity = velocity; Age = 0.0<s>; Alpha = 1.0}

let updateRockets (elapsed: float<s>) (aspectRatio : float) (particles : Particle list) = 
    decayAndUpdateParticles elapsed aspectRatio particles

    
