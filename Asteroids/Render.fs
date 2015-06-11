module Render

open Domain
open Ship
open Particles
open Geometry
open OpenTK.Graphics.OpenGL

let private renderColoredPoint (point : ColoredPoint2d) = 
    let c = point.Color
    let p = point.Point
    GL.Color4(c.R, c.G, c.B, 1.0); 
    GL.Vertex3(p.X,p.Y, 2.)

let renderShip (ship : Ship) = 
     // Draw triangle based on ship position
    PrimitiveType.Triangles |> GL.Begin
        
    let shipBody= 
        ship.BodyWrtOrigin 
        |> rotateColoredTriangleWrtOrigin ship.Heading 
        |> translateColoredTriangleByPoint ship.Position
    
    for point in shipBody.AsSeq do
        renderColoredPoint point

    GL.End()

    //Draw Ship Centre - Note: I've added this so you can see where the ship position is. 
//    PrimitiveType.Points |> GL.Begin
//    GL.PointSize(1.0f)
//    GL.Color3(1., 1., 1.); GL.Vertex3(ship.Position.X, ship.Position.Y, 2.) 
//    GL.End()

let private renderSprite (point: Point2d) (alpha: float) = 
    //GL.PushMatrix()
    GL.PointSize(5.0f)
    PrimitiveType.Points |> GL.Begin
    GL.Color4(0.2, 0.9, 1.0, alpha)
    GL.Vertex3(point.X, point.Y, 2.)
    GL.End()
    //GL.PopMatrix()

let renderParticles (state: GameState) = 
    for point in state.Particles do
        renderSprite point.Position point.Alpha

    for point in state.TrailParticles do
        renderSprite point.Position point.Alpha

let renderState (state: GameState) = 
    renderShip state.Ship
    renderParticles state
