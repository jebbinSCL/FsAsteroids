module Domain

open System
open System.Drawing

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open GlHelpers

type Point = { X: float32; Y: float32 }

type ColorPoint = {
    Color: GlColor    
    Point: Point
}

type Triangle = { V1: Point; V2: Point; V3: Point }

type Velocity = { 
    Direction: float
    Size: float
}
        
type Ship = { 
    Position: Point
    Velocity: Velocity 
}

// use a function to transform a Ship to a list of coloured points for passing to OpenGL
//let renderShip (s:Ship): ColorPoint list = ...