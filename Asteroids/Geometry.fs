module Geometry

open System
[<Measure>] type radian
[<Measure>] type degree

let convertDegreeToRadian (angle : float<degree>) : float<radian> = 
    let radiansPerDegree  = (Math.PI * 1.0<radian>) / 180.0<degree>
    radiansPerDegree * angle

type Point = { X: float; Y: float }
type Vector = { Dx: float; Dy: float }

let rotate angle v =
    let radians = float <| convertDegreeToRadian angle
    let cos = Math.Cos(radians)
    let sin = Math.Sin(radians)
    { Dx = v.Dx * cos - v.Dy * sin; Dy = v.Dx * sin + v.Dy * cos }


