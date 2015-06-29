open OpenTK
open OpenTK.Graphics
open DomainTypes


(*
    Tasks: - Test as much as possible!
*)

[<EntryPoint>]
let main _ = 
    let getAspectRatio (gw : GameWindow) = float gw.Width / float gw.Height
    use game = new GameWindow(960, 960, GraphicsMode.Default, "Asteroids")

    let initialAspectRatio = getAspectRatio game

    

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
            let timeUpdateStream = game.UpdateFrame |> Observable.map (fun args -> DomainTypes.TimeUpdate <| LanguagePrimitives.FloatWithMeasure args.Time)
            let aspectRatioStream = game.Resize |> Observable.map (fun _ -> DomainTypes.AspectRatioUpdate <| getAspectRatio game)
            Keyboard.createKeyboardTriggerStream game |> Observable.merge timeUpdateStream |> Observable.merge aspectRatioStream |> Observable.map GameEvent.StateChange
        
        let gameActionStream = 
            let renderFrameTriggerStream = game.RenderFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerRenderFrame)
            let updateFrameTriggerStream = game.UpdateFrame |> Observable.map (fun _ -> GameActionTrigger.TriggerUpdateFrame)
            renderFrameTriggerStream |> Observable.merge updateFrameTriggerStream |> Observable.map GameEvent.GameAction

        stateChangeStream 
        |> Observable.merge gameActionStream        
        |> Observable.scan DomainTransitions.processGameEvent (StateAction.UpdateState <| DomainTypes.initialGameState initialAspectRatio)

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
