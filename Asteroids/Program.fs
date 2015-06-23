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

    let load _ = GameConfig.onLoadSetup game

    let resize _ = GameConfig.onResizeSetup game

    let emitAspectRatio() = float game.Width / float game.Height

    let updateFrame (state: GameState) = 
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame (state: GameState)  = 
        GameConfig.preRenderConfigure()
        Render.renderState state
        game.SwapBuffers()

    use loadSubscription = game.Load.Subscribe load
    use resizeSubscription = game.Resize.Subscribe resize    

    let updatedStateStream = 
        let stateChangeStream = 
            let keyDownStream = game.KeyDown |> Observable.map Keyboard.transformKeyDown
            let keyUpStream = game.KeyUp |> Observable.map Keyboard.transformKeyUp
            let timeUpdateStream = game.UpdateFrame |> Observable.map (fun args -> Domain.TimeUpdate <| LanguagePrimitives.FloatWithMeasure args.Time)
            let aspectRatioStream = game.Resize |> Observable.map (fun _ -> Domain.AspectRatioUpdate <| emitAspectRatio())
            keyDownStream |> Observable.merge keyUpStream |> Observable.merge timeUpdateStream |> Observable.merge aspectRatioStream |> Observable.map GameEvent.StateChange
        
        let gameActionStream = 
            let renderFrameTriggerStream = game.RenderFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerRenderFrame)
            let updateFrameTriggerStream = game.RenderFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerUpdateFrame)
            renderFrameTriggerStream |> Observable.merge updateFrameTriggerStream |> Observable.map GameEvent.GameAction

        
        let gameEventStream = stateChangeStream |> Observable.merge gameActionStream

        gameEventStream
        |> Observable.scan Domain.processGameEvent (StateAction.UpdateState Domain.initialState)

    use renderFrameSubscription = 
        updatedStateStream
        |> Observable.choose(function | StateAction.RenderFrame state -> Some(state) | _ -> None)
        |> Observable.subscribe(fun state -> renderFrame state)

    use updateFrameSubscription = 
        updatedStateStream
        |> Observable.choose(function | StateAction.UpdateFrame state -> Some(state) | _ -> None)
        |> Observable.subscribe(fun state -> updateFrame state)
        
    game.Run(60.0)
    0 
