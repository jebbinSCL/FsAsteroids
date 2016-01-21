module Asteroids
open Geometry
open Physics
open Entities

type Asteroid = {
    Position: Point2d
    Velocity: Vector2d 
    BodyWrtOrigin : Point2d list 
    BoundingRadius : float
    BreakCount : int
    Age : float<s>
    Alpha : float
}

let randomInstance = new System.Random()

let createAsteroidBody boundingRadius =
    let pointCount = randomInstance.Next(5) + 5
    let angleUnit = 360.0<degree> / float pointCount
    let maxDistanceFromCenter = boundingRadius
    let randomFraction() = 
        randomInstance.NextDouble() * 0.7 + 0.3
    seq {0..pointCount-1}
    |> Seq.map(fun x -> rotatePointWrtOrigin (angleUnit * float x) {X = 0.0; Y = randomFraction() * maxDistanceFromCenter;} )
    |> Seq.toList

let createAsteroid (aspectRatio : float) (boundingRadius : float) = 
    let randomHeading = randomInstance.NextDouble() * 360.0<degree>
    let randLoc ratio = 
        let bound = positionMaximumValue ratio
        (randomInstance.NextDouble() * bound * 2.0) - (bound/2.0)
    let velocityMagintude = randomInstance.NextDouble() * 0.006 + 0.002
    let velocity = {Dx = 0.0; Dy = velocityMagintude} |> rotateVector randomHeading
    
    {Position= {X =randLoc aspectRatio; Y = randLoc neutralRatio }; Velocity = velocity; BodyWrtOrigin = createAsteroidBody boundingRadius; BoundingRadius = boundingRadius; BreakCount = 0; Age = 0.0<s>; Alpha= 1.0}

//TODO generate initial asteroids a certain distance from the ship for an easy start
let createInitialAsteroids (aspectRatio : float) = 
    let iniialBoundingRadius = 0.25
    seq {0..40} |> Seq.map (fun x ->createAsteroid aspectRatio iniialBoundingRadius) |> Seq.toList

let updateAsteroid aspectRatio (asteroid : Asteroid) = {asteroid with Position = Entities.updatePosition aspectRatio asteroid.Position asteroid.Velocity}

let updateAsteroidsList aspectRatio (asteroids : Asteroid list) = List.map (updateAsteroid aspectRatio) asteroids

let updateAsteroids (elapsed: float<s>) (aspectRatio : float) (asteroids : Asteroid list) = 
    updateAsteroidsList aspectRatio asteroids

let splitAsteroid (aspectRatio : float) (childrenCount: int) (splitFactor: float) (asteroid : Asteroid) = 
    let boundRadius = asteroid.BoundingRadius * splitFactor
    List.init childrenCount (fun _ -> createAsteroid aspectRatio boundRadius) 
    |> List.map (fun x -> {x with Position = asteroid.Position; BreakCount = asteroid.BreakCount + 1})

let breakAsteroids (aspectRatio : float) (asteroids : Asteroid list) = 
    let childrenCount = 2
    let splitFactor = 0.5
    asteroids |> List.collect (splitAsteroid aspectRatio childrenCount splitFactor)

let breakAsteroid (aspectRatio : float) (asteroid : Asteroid) = 
    let childrenCount = 2
    let splitFactor = 0.5
    splitAsteroid aspectRatio childrenCount splitFactor asteroid

let shatterAsteroids (aspectRatio : float) (asteroids : Asteroid list) = 
    let childrenCount = 12
    let splitFactor = 0.5 ** 2.0
    asteroids |> List.collect (splitAsteroid aspectRatio childrenCount splitFactor) 

let shatterAsteroid (aspectRatio : float) (asteroid : Asteroid) = 
    let childrenCount = 12
    let splitFactor = 0.5 ** 2.0
    splitAsteroid aspectRatio childrenCount splitFactor asteroid

let lifeSpan = 0.7<s> 

let updateShard (aspectRatio : float) asteroid = 
    let a = updateAsteroid aspectRatio asteroid
    {a with Alpha = 1.0 - a.Age / lifeSpan} 

let decayAndUpdateShards (elapsed: float<s>) (aspectRatio : float) (asteroids : Asteroid list) = 
    let aliveShards = asteroids |> List.map (fun p -> {p with Age=p.Age + elapsed}) |> List.filter(fun p -> p.Age < lifeSpan)
    aliveShards 
    |> List.map (updateShard aspectRatio)

let updateShards (elapsed: float<s>) (aspectRatio : float) (asteroids: Asteroid list) = 
    decayAndUpdateShards elapsed aspectRatio asteroids
    



