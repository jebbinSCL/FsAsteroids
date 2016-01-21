module Geometry

open System

[<Measure>] type radian
[<Measure>] type degree
//TODO Cleanup Geometry file
//TODO redo physics and Geometry and seperate out vector into magnitude and direction / rotation
//TODO use Units of measure everywhere
//TODO seperate out Triangle, point etc into own modules under root geometry module / namespace
//Rethink module orgainsation (See Nics?)


let radiansPerDegree  = (Math.PI * 1.0<radian>) / 180.0<degree>

let convertDegreeToRadian (angle : float<degree>) : float<radian> = radiansPerDegree * angle
let convertRadianToDegree (angle : float<radian>) : float<degree> = angle / radiansPerDegree

let constrainDegreeTo360 (angle : float<degree>) =
    let upperBoundary = 360.0<degree>
    let lowerBoundary = 0.0<degree>
    match angle with
    | tooLarge when angle > upperBoundary -> angle - upperBoundary
    | tooSmall when angle < lowerBoundary -> angle + upperBoundary
    | _ -> angle 

let randomDegreeAroundZero (random: Random) (range: float<degree>) = (random.NextDouble() * range) - (range/2.0)

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

let distanceBetweenPoints (p1 : Point2d) (p2 : Point2d) = 
    sqrt((p2.X - p1.X) ** 2.0 + (p2.Y - p1.Y) ** 2.0)

let addPointToColoredPoint (p : Point2d) (cp : ColoredPoint2d) = 
    {cp with Point = addPoints p cp.Point}

let addVectors (p1 : Vector2d) (p2 : Vector2d) = 
    {Dx = p1.Dx + p2.Dx; Dy = p1.Dy + p2.Dy }

let multiplyVector (v1: Vector2d) (multiplier: float) = 
    {Dx = v1.Dx * multiplier; Dy = v1.Dy * multiplier}

let dotProduct (p1 : Vector2d) (p2 : Vector2d) =
    p1.Dx * p2.Dx + p1.Dy * p2.Dy

let norm (v1 : Vector2d) = sqrt(v1.Dx ** 2.0 + v1.Dy ** 2.0 ) 

let angleBetweenVectors (p1 : Vector2d) (p2 : Vector2d) = 
    acos(dotProduct p1 p2 / (norm p1 * norm p2)) * 1.0<radian>

let rotatePointWrtOrigin (angle : float<degree>) (p : Point2d)  = 
    let radians = float <| convertDegreeToRadian angle
    let cosAngle = Math.Cos(radians)
    let sinAngle = Math.Sin(radians)
    {X= p.X * cosAngle - p.Y * sinAngle; Y= p.X * sinAngle + p.Y * cosAngle}

let rotateColoredPointWrtOrigin (angle : float<degree>) (p : ColoredPoint2d) = 
    {p with Point = rotatePointWrtOrigin angle p.Point}

let rotateColoredTriangleWrtOrigin (angle : float<degree>) (tri : ColoredTriangle2d) = 
    { P1 = rotateColoredPointWrtOrigin angle tri.P1
      P2 = rotateColoredPointWrtOrigin angle tri.P2
      P3 = rotateColoredPointWrtOrigin angle tri.P3 }

let translateColoredTriangleByPoint (point: Point2d) (tri : ColoredTriangle2d) = 
    { P1 = addPointToColoredPoint point tri.P1
      P2 = addPointToColoredPoint point tri.P2
      P3 = addPointToColoredPoint point tri.P3 }

let rotateVector angle v =
    let radians = float <| convertDegreeToRadian angle
    let cos = Math.Cos(radians)
    let sin = Math.Sin(radians)
    { Dx = v.Dx * cos - v.Dy * sin; Dy = v.Dx * sin + v.Dy * cos }



