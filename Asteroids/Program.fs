open OpenTK
open OpenTK.Graphics
open Domain

(*
    Tasks: - Test as much as possible!
*)

[<EntryPoint>]
let main _ = 
    use game = new GameWindow(960, 960, GraphicsMode.Default, "Asteroids")

    let load _ = GameConfig.onLoadSetup game

    let resize _ = GameConfig.onResizeSetup game

    let updateFrame (state: GameState) = 
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame (state: GameState)  = 
        GameConfig.preRenderConfigure()
        Render.renderState state
        game.SwapBuffers() 

    let updatedStateStream = 
        let stateChangeStream = 
            let keyStateTriggerStream = Keyboard.createKeyBoardStream game

            let timeUpdateStream = game.UpdateFrame |> Observable.map (fun args -> Domain.TimeUpdate <| LanguagePrimitives.FloatWithMeasure args.Time)
            let aspectRatioStream = game.Resize |> Observable.map (fun _ -> Domain.AspectRatioUpdate <| float game.Width / float game.Height)
            keyStateTriggerStream |> Observable.merge timeUpdateStream |> Observable.merge aspectRatioStream |> Observable.map GameEvent.StateChange
        
        let gameActionStream = 
            let renderFrameTriggerStream = game.RenderFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerRenderFrame)
            let updateFrameTriggerStream = game.UpdateFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerUpdateFrame)
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

    use loadSubscription = game.Load.Subscribe load
    use resizeSubscription = game.Resize.Subscribe resize  
        
    game.Run(60.0)
    0 
