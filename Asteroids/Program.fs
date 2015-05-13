open System

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Geometry
open Domain

[<EntryPoint>]
let main _ = 
    use game = new GameWindow(800, 600, GraphicsMode.Default, "Asteroids")

    let load _ =
        // Some game and OpenGL Setup
        game.VSync <- VSyncMode.On
        GL.Enable(EnableCap.Blend)
        GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)

    let resize _ = 
        //Setup of projection matrix for game
        GL.Viewport(game.ClientRectangle.X, game.ClientRectangle.Y, game.ClientRectangle.Width, game.ClientRectangle.Height)
        let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.), float32 game.Width / float32 game.Height, 0.001f, 5.0f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)

    let updateFrame (state :GameState) =
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame (state: GameState)  =

        //OpenGL Stuff to set view
        GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadMatrix(&modelview)

        // Draw triangle based on ship position
        PrimitiveType.Triangles |> GL.Begin
        let shipPos = state.Ship.Position
        (*Note the 4. (or 4.0) for the z coordinate of the vertices is 4, instead of zero because of the specific projection. 
            For now, simply keep it and abstract out the coordinates so that you can just use X and Y, while keeping Z contstant. 

            One other thing to note about the coordinates: The screen coordinate system is not between nice numbers. 
            I attempted to clean that up, but I've had no luck so far. 
         *) 
        GL.Color3(1., 0., 0.); GL.Vertex3(shipPos.X + -0.1, shipPos.Y + -0.1, 4.) 
        GL.Color3(1., 0., 0.); GL.Vertex3(shipPos.X + 0.1, shipPos.Y + -0.1, 4.)
        GL.Color3(0.2, 0.9, 1.); GL.Vertex3(shipPos.X + 0., shipPos.Y + 0.1, 4.)
        GL.End()

        // Game is double buffered
        game.SwapBuffers()

    //Handle keydownEvents and transform them into state changes 
    //Hint (To get the best behaviour, you may need to deal with key up, etc events)
    let keyDown (args: KeyboardKeyEventArgs) =
        match args.Key with
        | Key.Escape ->  UserStateChange.EndGame
        | Key.Up -> UserStateChange.ChangePosition {X = 0.0; Y = 0.01}
        | Key.Down -> UserStateChange.ChangePosition {X = 0.0; Y = -0.01}
        | Key.Right -> UserStateChange.ChangePosition {X = -0.01; Y = 0.0}
        | Key.Left -> UserStateChange.ChangePosition {X = 0.01; Y = 0.0}
        | _ -> UserStateChange.NoChange

    let updateGameState (state: GameState)  change = 
        match change with 
        | ChangePosition posChange-> 
            let pos = state.Ship.Position
            let newPos = {X = pos.X + posChange.X; Y = pos.Y + posChange.Y}
            printfn "%A" newPos
            let newShip = {state.Ship with Position = newPos}
            {state with Ship = newShip}
        | EndGame -> {state with Running=Stop}
        | NoChange -> state

    use loadSubscription = game.Load.Subscribe load
    use resizeSubscription = game.Resize.Subscribe resize 

    (*Below, the game state is being stored in a reference cell instead of being passed through the observable functions
        The reason is that my original approach that passed the state on directly caused a memory leak. 
        I'm not sure if it was due to my original code, or an issue with the library, but this a simple alternative that means the code is nearly the same,
        with a little local mutability.
        
        Also, reference cell is used instead of a mutable value because mutable values cannot be captured by lambdas. 
        For a longer explaination see: https://lorgonblog.wordpress.com/2008/11/12/on-lambdas-capture-and-mutability/ 
        Also msdn reference : https://msdn.microsoft.com/en-us/library/dd233186.aspx*)
    let currentGameState = ref Domain.initialState

    use updateGameStateSub = 
        game.KeyDown
        |> Observable.map keyDown
        |> Observable.scan updateGameState Domain.initialState 
        |> Observable.subscribe (fun state -> currentGameState := state)

    use renderFrameSub = 
        game.RenderFrame
        |> Observable.subscribe(fun _ -> renderFrame !currentGameState)

    use updateFrameSub = 
        game.UpdateFrame
        |> Observable.subscribe(fun _ -> updateFrame !currentGameState)
        
    game.Run(60.0)

    0 
