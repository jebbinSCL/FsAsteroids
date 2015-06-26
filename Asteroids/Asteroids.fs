module Asteroids
open Geometry
open Physics
open Entities

type Asteroid = {
    Position: Point2d
    Velocity: Vector2d 
    BodyWrtOrigin : Point2d list 
}

let randomInstance = new System.Random()

let createAsteroidBody() =
    let pointCount = randomInstance.Next(5) + 5
    let angleUnit = 360.0<degree> / float pointCount
    let maxDistanceFromCenter = 0.25
    let randomFraction() = 
        randomInstance.NextDouble() * 0.7 + 0.3
    seq {0..pointCount-1}
    |> Seq.map(fun x -> rotatePointWrtOrigin (angleUnit * float x) {X = 0.0; Y = randomFraction() * maxDistanceFromCenter;} )
    |> Seq.toList


let createAsteroid (aspectRatio : float)= 
    let randomHeading = randomInstance.NextDouble() * 360.0<degree>
    let randLoc ratio = 
        let bound = positionMaximumValue ratio
        (randomInstance.NextDouble() * bound * 2.0) - (bound/2.0)

    let velocityMagintude = randomInstance.NextDouble() * 0.006 + 0.002
    let velocity = {Dx = 0.0; Dy = velocityMagintude} |> rotateVector randomHeading
    {Position= {X =randLoc aspectRatio; Y = randLoc neutralRatio }; Velocity = velocity; BodyWrtOrigin = createAsteroidBody()}

let updateAsteroid aspectRatio (asteroid : Asteroid) = {asteroid with Position = Entities.updatePosition aspectRatio asteroid.Position asteroid.Velocity}

let updateAsteroidsList aspectRatio (asteroids : Asteroid list) = List.map (updateAsteroid aspectRatio) asteroids

let updateAsteroids (elapsed: float<s>) (aspectRatio : float) (asteroids : Asteroid list) = 
    let asteroids' = if List.length asteroids < 10 then createAsteroid aspectRatio :: asteroids else asteroids
    updateAsteroidsList aspectRatio asteroids'

