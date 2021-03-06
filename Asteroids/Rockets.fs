﻿module Rockets

open Geometry
open Physics
open Particles

let lifeSpan = 0.8<s>

let createRocket (shipPos: Point2d) (shipHeading: float<degree>) = 
    let velocity = {Dx = 0.0; Dy = 0.075} |> rotateVector shipHeading
    {Position=shipPos; Velocity = velocity; Age = 0.0<s>; Alpha = 1.0}

let updateParticle (aspectRatio : float) particle = 
    updatePosition aspectRatio particle

let updateRockets (elapsed: float<s>) (aspectRatio : float) (particles : Particle list) = 
    decayParticles elapsed lifeSpan particles
    |> List.map (updateParticle aspectRatio)

    
