module Keyboard

open Physics
open Geometry
open Domain
open OpenTK
open OpenTK.Input

type KeyState = 
    | KeyPressed 
    | KeyReleased

type PairedKeyOptions = 
    | PositiveKey of KeyState
    | NegativeKey of KeyState

type PairedKeyState<'a, 'b> = {
    PositiveKeyState : 'a
    NegativeKeyState : 'a
    Result : 'b
}

let defaultPairedKeyState (defaultResult: 'a) = {PositiveKeyState = KeyReleased; NegativeKeyState = KeyReleased; Result = defaultResult}

let transformKeyDown = function
    | Key.T -> ToggleTrail
    | Key.P -> ToggleParticles
    | Key.Space -> FireRocket
    | Key.Escape ->  EndGame
    | _ -> NoChange


let transformKeyUp = function
    | _ -> NoChange

let createKeyboardTriggerStream (game : GameWindow) = 
    let mergeTuple a (b,c) = a,b,c
    let getKey (args: KeyboardKeyEventArgs) = args.Key
    let downAccelStream, downHeadingStream, otherDownStream = 
        game.KeyDown
        |> Observable.map getKey
        |> Observable.split (fun key -> match key with | Key.Up -> Choice1Of2 <| PositiveKey KeyPressed  | Key.Down -> Choice1Of2 <| NegativeKey KeyPressed | other -> Choice2Of2 other) 
        |> (fun (accelStream , otherStream) -> Observable.split (fun key -> match key with | Key.Right -> Choice1Of2 <| PositiveKey KeyPressed | Key.Left -> Choice1Of2 <| NegativeKey KeyPressed | other -> Choice2Of2 other) otherStream |> mergeTuple accelStream)

    let upAccelStream, upHeadingStream, otherUpStream = 
        game.KeyUp
        |> Observable.map getKey
        |> Observable.split (fun key -> match key with | Key.Up -> Choice1Of2 <| PositiveKey KeyReleased  | Key.Down -> Choice1Of2 <| NegativeKey KeyReleased | other -> Choice2Of2 other) 
        |> (fun (accelStream , otherStream) -> Observable.split (fun key -> match key with | Key.Right -> Choice1Of2 <| PositiveKey KeyReleased | Key.Left -> Choice1Of2 <| NegativeKey KeyReleased | other -> Choice2Of2 other) otherStream |> mergeTuple accelStream)

    let processActions (choices: 'a*'a*'a*'a) (state : PairedKeyState<KeyState,'a>) (nextKey : PairedKeyOptions) = 
        let state' = 
            match nextKey with
            | PositiveKey keyState -> {state with PositiveKeyState = keyState}
            | NegativeKey keyState -> {state with NegativeKeyState = keyState}
        let result = 
            let c1,c2,c3,c4 = choices
            match state'.PositiveKeyState, state'.NegativeKeyState with 
            | KeyReleased, KeyReleased -> c1
            | KeyPressed, KeyPressed -> c2
            | KeyPressed, KeyReleased -> c3
            | KeyReleased, KeyPressed -> c4
        {state' with Result = result}

    let accelStream = 
        let processAccelActions = processActions (Neutral,Positive {Dx = 0.0; Dy = 0.0},Positive {Dx = 0.0; Dy = 0.002},Negative {Dx = 0.0; Dy = 0.002})
        downAccelStream |> Observable.merge upAccelStream 
        |> Observable.scan processAccelActions (defaultPairedKeyState Neutral)
        |> Observable.map (fun x -> ChangeAcceleration x.Result)
    let headingStream = 
        let processRotAccelActions = processActions (Physics.neutralRotationalVelocity,Physics.neutralRotationalVelocity,5.0<degree>,-5.0<degree>)
        downHeadingStream |> Observable.merge upHeadingStream
        |> Observable.scan processRotAccelActions (defaultPairedKeyState Physics.neutralRotationalVelocity)
        |> Observable.map (fun x -> ChangeHeading x.Result)
    let otherKeyStream = Observable.merge (Observable.map transformKeyUp otherUpStream) (Observable.map transformKeyDown otherDownStream)
    accelStream |> Observable.merge headingStream |> Observable.merge otherKeyStream
    


