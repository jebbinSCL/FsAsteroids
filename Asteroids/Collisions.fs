module Collisions

open Geometry
open Domain
open Ship
open Asteroids
open Particles
open System.Collections.Generic

let particleAsteroidTest (asteroid: Asteroid) (particle: Particle)  = 
    distanceBetweenPoints particle.Position asteroid.Position < asteroid.BoundingRadius

let shipAsteroidTest (ship: Ship) (asteroid: Asteroid)   = 
    distanceBetweenPoints ship.Position asteroid.Position < asteroid.BoundingRadius
    
let detectParticleCollisions (state: GameState) = 
    let mutable survivingRockets = List<Particle>(state.Rockets)
    let mutable newAsteroids = List<Asteroid>()
    let mutable destroyedAsteroids = List<Asteroid>()

    for asteroid in state.Asteroids do 
        let result = Seq.tryFind (particleAsteroidTest asteroid) survivingRockets
        match result with
        |Some particle -> 
            survivingRockets.Remove(particle) |> ignore
            if asteroid.BreakCount = 0 then
                newAsteroids.AddRange( breakAsteroid state.AspectRatio asteroid )
            else 
                destroyedAsteroids.AddRange(shatterAsteroid state.AspectRatio asteroid)

        |None -> newAsteroids.Add(asteroid)


    {state with Rockets = List.ofSeq survivingRockets ; Asteroids = List.ofSeq newAsteroids; Shards =state.Shards @ List.ofSeq  destroyedAsteroids}
                
    //TODO
let detectShipCollisions (state: GameState) = 
    let result = Seq.tryFind (shipAsteroidTest state.Ship) state.Asteroids
    match result with
    | Some asteroid -> 
        async {printfn "Ship Collision!\n"} |> Async.Start
        state
    | None -> state
    
    

let detectCollisions (state: GameState) = 
    if state.Asteroids.IsEmpty then
        state
    else 
        let state' = if not state.Rockets.IsEmpty then detectParticleCollisions state else state
        detectShipCollisions state'