// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.Drawing

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

type GlColor = {Red:float32 ; Green:float32; Blue:float32; Alpha:float32}

type GameRunning =
    | Continue
    | Stop

type GameState = {
    Running : GameRunning
    BackgroundColor : GlColor
}

type StateChange = 
    | StartGame
    | ChangeBackgroundColour of GlColor
    | EndGame
    | NoChange

[<EntryPoint>]
let main argv = 
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
        GL.Color3(1.f, 1.f, 0.f); GL.Vertex3(-1.f, -1.f, 4.f)
        GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(1.f, -1.f, 4.f)
        GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(0.f, 1.f, 4.f)
        GL.End()

        game.SwapBuffers()

    let random = new Random()
    let randomGlColour() = {Red= float32 <| random.NextDouble(); Green =  float32 <| random.NextDouble(); Blue= float32 <| random.NextDouble(); Alpha = 0.0f}

    let keyDown (args: KeyboardKeyEventArgs) =
        let scale = if args.Shift then 10 else 1
        match args.Key with
        | Key.Escape ->  StateChange.EndGame
        | Key.KeypadMinus -> StateChange.ChangeBackgroundColour <| randomGlColour()
        | _ -> StateChange.NoChange

    let updateGameState (state: GameState)  change = 
        match change with 
        | StartGame -> state
        | ChangeBackgroundColour color -> {state with BackgroundColor = color}
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

        let initialState = 
            { Running = Continue; BackgroundColor =randomGlColour() }

        let updateGameStateStream = 
            game.KeyDown
            |> Observable.map keyDown
            |> Observable.merge startGameObservable
            |> Observable.scan updateGameState initialState  

        let currentGameState = ref initialState

        let updateCurrentStateSub = updateGameStateStream |> Observable.subscribe (fun state -> currentGameState := state)

        let renderFrameSub = 
            game.RenderFrame
            |> Observable.subscribe(fun event -> renderFrame !currentGameState)

        let updateFrameSub = 
            game.UpdateFrame
            |> Observable.subscribe(fun event -> updateFrame !currentGameState)
        triggerGameStart

    startGameSubscription()
        
    game.Run(60.0)
    0 
