module Render

open Domain
open Ship
open Particles
open Asteroids
open Geometry
open OpenTK.Graphics.OpenGL

let private renderColoredPoint (point : ColoredPoint2d) = 
    let c = point.Color
    let p = point.Point
    GL.Color4(c.R, c.G, c.B, 1.0); 
    GL.Vertex3(p.X,p.Y, 2.)

let private renderPoint (point : Point2d) = 
    GL.Color4(1.0, 1.0, 1.0, 1.0); 
    GL.Vertex3(point.X,point.Y, 2.)

let private renderPointWithColor (color : Color3) (point : Point2d) = 
    GL.Color4(color.R, color.G, color.B, 1.0); 
    GL.Vertex3(point.X,point.Y, 2.)

let renderShip (ship : Ship) = 
    PrimitiveType.Triangles |> GL.Begin
        
    let shipBody= 
        ship.BodyWrtOrigin 
        |> rotateColoredTriangleWrtOrigin ship.Heading 
        |> translateColoredTriangleByPoint ship.Position
    
    for point in shipBody.AsSeq do
        renderColoredPoint point

    GL.End()


let private renderSprite (point: Point2d) (alpha: float) = 
    GL.PointSize(3.0f)
    PrimitiveType.Points |> GL.Begin
    GL.Color4(0.2, 0.9, 1.0, alpha)
    GL.Vertex3(point.X, point.Y, 2.)
    GL.End()

let renderParticles (state: GameState) = 
    for point in state.Particles do
        renderSprite point.Position point.Alpha

    for point in state.TrailParticles do
        renderSprite point.Position point.Alpha

    for point in state.Rockets do 
        renderSprite point.Position point.Alpha

let asteroidColor = {R=0.2; G=0.2; B=0.2}
let renderAsteroid (asteroid : Asteroid) = 
    PrimitiveType.Polygon |> GL.Begin
    let asteroidBody = 
        asteroid.BodyWrtOrigin
        |> List.map (addPoints asteroid.Position)

    for point in asteroidBody do
        renderPointWithColor asteroidColor point

    GL.End()

let renderAsteroids (asteroids : Asteroid list) = 
    for asteroid in asteroids do
        renderAsteroid asteroid

let renderState (state: GameState) = 
    renderShip state.Ship
    renderParticles state
    renderAsteroids state.Asteroids
