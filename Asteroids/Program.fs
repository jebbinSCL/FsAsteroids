open OpenTK
open OpenTK.Graphics

open Domain

(*
    References: 
    http://www.opentk.com/
    Reactive Programming intro with observables and f# http://fsharpforfunandprofit.com/posts/concurrency-reactive/ 
    Reference cells and "mutable values cannot be capturred by lambdas" https://lorgonblog.wordpress.com/2008/11/12/on-lambdas-capture-and-mutability/ 
    Reference Cells : https://msdn.microsoft.com/en-us/library/dd233186.aspx

    Tasks:
        - Refactor the open GL Program.fs - Done
        - Reach functionality goal of : 
            Deal with basic ship movement: 
                1. Left and Right arrow should rotate the ship
                2. Forward arrow should cause acceleration
                3. Backwards arrow should cause deceleration
                4. Asteroids style position wrap around when an object leaves the screen. PLay the asteroids game if you cant remember it!
        - Test as much as possible!
*)

[<EntryPoint>]
let main _ = 
    use game = new GameWindow(960, 960, GraphicsMode.Default, "Asteroids")

    let load _ = GameConfig.loadConfig game

    let resize _ = GameConfig.resizeConfig game

    let updateFrame (state: GameState) = 
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame state  = 
        GameConfig.renderConfig()
        Render.renderState state
        game.SwapBuffers()

    (*Below, the game state is being stored in a reference cell instead of being passed through the observable functions
    The reason is that my original approach that passed the state on directly caused a memory leak. 
    I'm not sure if it was due to my original code, or an issue with the library, but this a simple alternative that means the code is nearly the same,
    with a little local mutability. *)
        
    let gameState = ref Domain.initialState

    use loadSubscription = game.Load.Subscribe load
    use resizeSubscription = game.Resize.Subscribe resize      

    use updateTriggerSubscription = 
        let keyDownStream = game.KeyDown |> Observable.map Domain.transformKeyDown
        let keyUpStream = game.KeyUp |> Observable.map Domain.transformKeyUp

        let combinedUserInputStreams = keyDownStream |> Observable.merge keyUpStream

        let timeUpdateStream = game.UpdateFrame |> Observable.map (fun _ -> Domain.TimeUpdate)

        let updateStreams = combinedUserInputStreams |> Observable.merge timeUpdateStream

        updateStreams
        |> Observable.scan Domain.updateGameState !gameState
        |> Observable.subscribe (fun newState -> gameState := newState)

    use renderFrameSubscription = 
        game.RenderFrame
        |> Observable.subscribe(fun _ -> renderFrame !gameState)

    use updateFrameSubscription = 
        game.UpdateFrame
        |> Observable.subscribe(fun _ -> updateFrame !gameState)
        
    game.Run(60.0)
    0 
