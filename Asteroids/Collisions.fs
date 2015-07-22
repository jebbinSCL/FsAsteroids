module Collisions

open Geometry
open Domain
open Ship
open Asteroids
open Particles
open System.Collections.Generic


let particleHasHitAsteroid (asteroid: Asteroid) (particle: Particle)  = 
    distanceBetweenPoints particle.Position asteroid.Position < asteroid.BoundingRadius

let shipHasHitAsteroid (ship: Ship) (asteroid: Asteroid)   = 
    distanceBetweenPoints ship.Position asteroid.Position < asteroid.BoundingRadius
    
let detectParticleCollisions (state: GameState) = 
    let mutable survivingRockets = List<Particle>(state.Rockets)
    let mutable newAsteroids = List<Asteroid>()
    let mutable destroyedAsteroids = List<Asteroid>()
    for asteroid in state.Asteroids do 
        let result = Seq.tryFind (particleHasHitAsteroid asteroid) survivingRockets
        match result with
        |Some particle -> 
            survivingRockets.Remove(particle) |> ignore
            if asteroid.BreakCount = 0 then
                newAsteroids.AddRange( breakAsteroid state.AspectRatio asteroid )
            else 
                destroyedAsteroids.AddRange(shatterAsteroid state.AspectRatio asteroid)
        |None -> newAsteroids.Add(asteroid)

    {state with Rockets = List.ofSeq survivingRockets ; Asteroids = List.ofSeq newAsteroids; Shards =state.Shards @ List.ofSeq  destroyedAsteroids}

let detectParticleCollisionsRec (state: GameState) = 
    let rec detectPCol (unProcessedAsteroids : Asteroid list) (rockets : Particle list) (newAsteroids : Asteroid list)  (destroyedAsteroids: Asteroid list) = 
        match unProcessedAsteroids with
        | asteroid :: rest ->
            let hitRockets, survivingRockets = List.partition (particleHasHitAsteroid asteroid) rockets
            match hitRockets with 
            | [] -> detectPCol rest survivingRockets (asteroid :: newAsteroids) destroyedAsteroids
            | _ -> 
                if asteroid.BreakCount = 0 then
                    detectPCol rest survivingRockets (breakAsteroid state.AspectRatio asteroid @ newAsteroids) destroyedAsteroids
                else 
                    detectPCol rest survivingRockets newAsteroids (shatterAsteroid state.AspectRatio asteroid @ destroyedAsteroids)
        | [] -> rockets, newAsteroids, destroyedAsteroids
        
    let survivingRockets, newAsteroids, destroyedAsteroids = detectPCol state.Asteroids state.Rockets [] state.Shards

    {state with Rockets = survivingRockets ; Asteroids = newAsteroids; Shards = destroyedAsteroids}

let detecthParticleCollisonsFold (state: GameState) = 
    let collisionFolder ((rockets,newAsteroids,destroyedAsteroids) : Particle list * Asteroid list * Asteroid list) (asteroid : Asteroid) = 
        let hitRockets, survivingRockets = List.partition (particleHasHitAsteroid asteroid) rockets
        match hitRockets with 
        | [] -> survivingRockets, (asteroid :: newAsteroids), destroyedAsteroids
        | _ -> 
            if asteroid.BreakCount = 0 then
                survivingRockets, (breakAsteroid state.AspectRatio asteroid @ newAsteroids), destroyedAsteroids
            else 
                survivingRockets, newAsteroids, (shatterAsteroid state.AspectRatio asteroid @ destroyedAsteroids)

    let survivingRockets, newAsteroids, destroyedAsteroids = List.fold collisionFolder (state.Rockets, [], state.Shards) state.Asteroids

    {state with Rockets = survivingRockets ; Asteroids = newAsteroids; Shards = destroyedAsteroids}

    //TODO Complete ship collisions
let detectShipCollisions (state: GameState) = 
    let result = Seq.tryFind (shipHasHitAsteroid state.Ship) state.Asteroids
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