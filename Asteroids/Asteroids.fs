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
    let xLoc = 
        let xBound = positionBoundary aspectRatio
        (randomInstance.NextDouble() * xBound * 2.0) - (xBound/2.0)
    let yLoc = 
        let yBound = positionBoundary neutralRatio
        (randomInstance.NextDouble() * yBound * 2.0) - (yBound/2.0)

    let velocityMagintude = randomInstance.NextDouble() * 0.006 + 0.002
    let velocity = {Dx = 0.0; Dy = velocityMagintude} |> rotateVector randomHeading
    {Position= {X =xLoc; Y = yLoc }; Velocity = velocity; BodyWrtOrigin = createAsteroidBody()}

let updateAsteroid aspectRatio (asteroid : Asteroid) = 
    {asteroid with Position = Entities.updatePosition aspectRatio asteroid.Position asteroid.Velocity}
    

let updateAsteroidsList aspectRatio (asteroids : Asteroid list) = 
    asteroids 
    |> List.map (updateAsteroid aspectRatio)

let updateAsteroids (elapsed: float<s>) (aspectRatio : float) (asteroids : Asteroid list) = 
    if List.length asteroids < 10 then
        let asteroids' = createAsteroid aspectRatio :: asteroids
        updateAsteroidsList aspectRatio asteroids'
    else 
        updateAsteroidsList aspectRatio asteroids

