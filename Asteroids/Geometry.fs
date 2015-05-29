module Geometry

open System
[<Measure>] type radian
[<Measure>] type degree

let radiansPerDegree  = (Math.PI * 1.0<radian>) / 180.0<degree>

let convertDegreeToRadian (angle : float<degree>) : float<radian> = radiansPerDegree * angle

type Vector2d = { Dx: float; Dy: float }

type Point2d = { X: float; Y: float }
type Triangle2d = { P1:Point2d; P2:Point2d; P3:Point2d} with
    member tri.AsSeq = 
        seq {
            yield tri.P1
            yield tri.P2
            yield tri.P3
        }

type Color3 = {R: float; G:float; B:float}
type ColoredPoint2d = {
    Color : Color3
    Point : Point2d
}

type ColoredTriangle2d = { P1:ColoredPoint2d; P2:ColoredPoint2d; P3:ColoredPoint2d} with
    member tri.AsSeq = 
        seq {
            yield tri.P1
            yield tri.P2
            yield tri.P3
        }


let addPoints (p1 : Point2d) (p2 : Point2d) = 
    {X = p1.X + p2.X; Y = p1.Y + p2.Y }

let addVectors (p1 : Vector2d) (p2 : Vector2d) = 
    {Dx = p1.Dx + p2.Dx; Dy = p1.Dy + p2.Dy }

let rotatePointWrtOrigin (angle : float<degree>) (p : Point2d)  = 
    let radians = float <| convertDegreeToRadian angle
    let cosAngle = Math.Cos(radians)
    let sinAngle = Math.Sin(radians)
    {X= p.X * cosAngle - p.Y * sinAngle; Y= p.X * sinAngle + p.Y * cosAngle}

let rotateColoredPointWrtOrigin (angle : float<degree>) (p : ColoredPoint2d) = 
    {p with Point = rotatePointWrtOrigin angle p.Point}

let rotate angle v =
    let radians = float <| convertDegreeToRadian angle
    let cos = Math.Cos(radians)
    let sin = Math.Sin(radians)
    { Dx = v.Dx * cos - v.Dy * sin; Dy = v.Dx * sin + v.Dy * cos }


