module Render

open Domain
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
    let shipBody = 
        ship.BodyWrtOrigin.AsSeq 
        |> Seq.map (fun x -> {x with Point = rotatePointWrtOrigin ship.Heading x.Point |> addPoints ship.Position})  
    
    for point in shipBody do
        renderColoredPoint point

    GL.End()

    //Draw Ship Centre - Note: I've added this so you can see where the ship position is. 
    PrimitiveType.Points |> GL.Begin

    GL.Color3(1., 1., 1.); GL.Vertex3(ship.Position.X, ship.Position.Y, 2.) 
    GL.End()

let renderState (state: GameState) = 
    renderShip state.Ship
