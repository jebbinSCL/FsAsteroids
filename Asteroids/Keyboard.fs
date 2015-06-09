module Keyboard

open Physics
open Geometry
open Domain
open OpenTK.Input

let transformKeyDown (args: KeyboardKeyEventArgs) =
    match args.Key with
    | Key.Escape ->  EndGame
    | Key.Up -> StartAcceleration <| Positive {Dx = 0.0; Dy = 0.002}
    | Key.Down -> StartAcceleration <| Negative {Dx = 0.002; Dy = 0.002}
    | Key.Right -> StartHeadingChange 5.0<degree>
    | Key.Left -> StartHeadingChange -5.0<degree>
    | Key.P -> ToggleParticles
    | _ -> NoChange

let transformKeyUp (args: KeyboardKeyEventArgs) = 
    match args.Key with
    | Key.Up -> StopAcceleration
    | Key.Down -> StopAcceleration 
    | Key.Right -> StopHeadingChange
    | Key.Left -> StopHeadingChange
    | _ -> NoChange

