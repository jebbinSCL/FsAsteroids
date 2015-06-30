module Collisions

open Geometry
open Domain
open Asteroids
open Particles
open System.Collections.Generic

let particleAsteroidTest (asteroid: Asteroid) (particle: Particle)  = 
    distanceBetweenPoints particle.Position asteroid.Position < asteroid.BoundingRadius
    
let asteroidTest (particles: Particle seq) (asteroid: Asteroid) = 
    Seq.tryFind (particleAsteroidTest asteroid) particles
    

let detectParticleCollisions (state: GameState) = 
    let mutable survivingRockets = List<Particle>(state.Rockets)
    let mutable newAsteroids = List<Asteroid>()
    let mutable destroyedAsteroids = List<Asteroid>()

    for asteroid in state.Asteroids do 
        let result = asteroidTest survivingRockets asteroid
        match result with
        |Some particle -> 
            survivingRockets.Remove(particle) |> ignore
            if asteroid.BreakCount = 0 then
                newAsteroids.AddRange( breakAsteroid state.AspectRatio asteroid )
            else 
                destroyedAsteroids.AddRange(shatterAsteroid state.AspectRatio asteroid)

        |None -> newAsteroids.Add(asteroid)


    {state with Rockets = List.ofSeq survivingRockets ; Asteroids = List.ofSeq newAsteroids; Shards =state.Shards @ List.ofSeq  destroyedAsteroids}
                
//let detectShipCollisions (state: GameState) = 
    
    

let detectCollisions (state: GameState) = 
    if state.Asteroids.IsEmpty then
        state
    else 
        let state' = if not state.Rockets.IsEmpty then detectParticleCollisions state else state
        state'
        //detectShipCollisions state'