﻿open System
open System.Drawing

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open GlHelpers
open Domain

type GameRunning =
    | Continue
    | Stop

type GameState = {
    Running : GameRunning
    BackgroundColor : GlColor
}

type StateChange = 
    | StartGame
    | ChangeBackgroundColor of GlColor
    | EndGame
    | NoChange

let initialState = { 
    Running = Continue
    BackgroundColor = {Red= 0.0f; Green =  0.0f; Blue= 0.0f; Alpha = 0.0f}
}

let randomGlColor = 
    let random = new Random()
    fun () -> {Red= float32 <| random.NextDouble(); Green =  float32 <| random.NextDouble(); Blue= float32 <| random.NextDouble(); Alpha = 0.0f}

[<EntryPoint>]
let main _ = 
    use game = new GameWindow(800, 600)
    game.Title <- "Asteroids"

    let load _ = 
        game.VSync <- VSyncMode.On
        GL.Enable(EnableCap.DepthTest)

    let resize _ = 
        GL.Viewport(game.ClientRectangle.X, game.ClientRectangle.Y, game.ClientRectangle.Width, game.ClientRectangle.Height)
        let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.), float32 game.Width / float32 game.Height, 1.f, 64.f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)

    let updateFrame (state :GameState) =
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame (state: GameState)  =
        GL.ClearColor(state.BackgroundColor.Red, state.BackgroundColor.Green, state.BackgroundColor.Blue, 0.0f)

        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadMatrix(&modelview)

        PrimitiveType.Triangles |> GL.Begin
        GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(-0.1f, 0.1f, 4.f)
        GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(0.1f, 0.1f, 4.f)
        GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(0.f, 0.3f, 4.f)
        GL.End()

        game.SwapBuffers()

    let keyDown (args: KeyboardKeyEventArgs) =
        let scale = if args.Shift then 10 else 1
        match args.Key with
        | Key.Escape ->  StateChange.EndGame
        | Key.KeypadMinus -> StateChange.ChangeBackgroundColor <| randomGlColor()
        | _ -> StateChange.NoChange

    let updateGameState (state: GameState)  change = 
        match change with 
        | StartGame -> state
        | ChangeBackgroundColor color -> {state with BackgroundColor = color}
        | EndGame -> {state with Running=Stop}
        | NoChange -> state

    let loadSubscription = game.Load.Subscribe load
    let resizeSubscription = game.Resize.Subscribe resize

    let startGameSubscription = 

        let startGameObservable,triggerGameStart = 
            let internalEvent = new Event<StateChange>()
            let observable = internalEvent.Publish
            let trigger() = internalEvent.Trigger(StartGame)
            (observable,trigger)

        let updateGameStateStream = 
            game.KeyDown
            |> Observable.map keyDown
            |> Observable.merge startGameObservable
            |> Observable.scan updateGameState initialState  

        let currentGameState = ref initialState
        let updateCurrentStateSub = updateGameStateStream |> Observable.subscribe (fun state -> currentGameState := state)

        let renderFrameSub = 
            game.RenderFrame
            |> Observable.subscribe(fun _ -> renderFrame !currentGameState)

        let updateFrameSub = 
            game.UpdateFrame
            |> Observable.subscribe(fun _ -> updateFrame !currentGameState)
        triggerGameStart

    startGameSubscription()
        
    game.Run(60.0)
    0 
