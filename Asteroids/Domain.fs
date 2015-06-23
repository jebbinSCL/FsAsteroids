module Domain

open Geometry
open Physics
open Ship
open TrailParticles
open Particles

type GameRunning =
    | Continue
    | Stop   

type GameState = {
    Running : GameRunning
    AspectRatio : float
    TrailIsActive : bool
    ParticlesAreActive : bool
    Ship : Ship
    TrailParticles : Particle list
    Particles : Particle list
    Rockets : Particle list
}

let initialState = { 
    Running = Continue
    AspectRatio = 1.0
    TrailIsActive = false
    ParticlesAreActive = false
    Ship = Ship.initialShip
    TrailParticles = []
    Particles = []
    Rockets = []
}

type StateChangeTrigger = 
    | EndGame
    | StartAcceleration of Acceleration
    | StopAcceleration
    | StartHeadingChange of float<degree>
    | StopHeadingChange
    | ToggleTrail
    | ToggleParticles
    | FireRocket
    | TimeUpdate of float<s>
    | AspectRatioUpdate of float
    | NoChange

type GameActionTrigger = 
    | TriggerRenderFrame
    | TriggerUpdateFrame

type GameEvent = 
    | StateChange of StateChangeTrigger
    | GameAction of GameActionTrigger

type StateAction = 
    | UpdateState of GameState
    | RenderFrame of GameState
    | UpdateFrame of GameState

let extractState (action : StateAction) = 
    match action with 
    | UpdateState state | RenderFrame state | UpdateFrame state -> state

let updateHeading state = {state with Ship = Ship.updateHeading state.Ship}

let updateVelocity state = {state with Ship = Ship.updateVelocity state.Ship}

let updatePosition (aspectRatio : float) state = {state with Ship = Ship.updatePosition aspectRatio state.Ship} 

let updateTrail (elapsed : float<s>) (aspectRatio : float) (state: GameState) = 
    let trail = TrailParticles.updateParticles state.TrailParticles elapsed aspectRatio state.Ship.Position state.Ship.Thrust state.Ship.Heading
    {state with TrailParticles = trail} 

let updateParticles (elapsed : float<s>) (aspectRatio : float) (state: GameState) = 
    let particles = Particles.updateParticles state.Particles elapsed aspectRatio state.Ship.Position state.Ship.Thrust state.Ship.Heading
    {state with Particles = particles} 

let updateRockets (elapsed : float<s>) (aspectRatio : float) (state: GameState) = 
    let rockets = Rockets.updateRockets elapsed aspectRatio state.Rockets
    {state with Rockets = rockets}

let updateStateWithTime (oldState: GameState)  (elapsed : float<s>) = 
    let updateIf condition transform value = if condition value then transform value else value
    //TODO rather than pass aspect ratio to everything, fix value in updatePos function and pass function
    oldState 
    |> updateIf (fun state -> state.Ship.RotationalVelocity <> Ship.neutralRotationalVelocity) updateHeading
    |> updateIf (fun state -> state.Ship.Thrust <> Ship.neutralThrust) updateVelocity
    |> updateIf (fun state -> state.Ship.Velocity <> Ship.neutralVelocity) (updatePosition oldState.AspectRatio)
    |> updateIf (fun state -> state.ParticlesAreActive) (updateParticles elapsed oldState.AspectRatio)
    |> updateIf (fun state -> state.TrailIsActive) (updateTrail elapsed oldState.AspectRatio)
    |> updateRockets elapsed oldState.AspectRatio

let updateGameState stateAction change = 
    let state = extractState stateAction
    let newState = 
        match change with 
        | StartAcceleration newThrust-> 
            {state with Ship = Ship.updateThrust newThrust state.Ship }
        | StopAcceleration ->  
            {state with Ship = Ship.updateThrust Ship.neutralThrust state.Ship }
        | StartHeadingChange newRotationalVelocity->  
            {state with Ship = Ship.updateRotationalVelocity newRotationalVelocity state.Ship }
        | StopHeadingChange -> 
            {state with Ship = Ship.updateRotationalVelocity Ship.neutralRotationalVelocity state.Ship }
        | ToggleTrail ->
            {state with TrailIsActive = not state.TrailIsActive; TrailParticles = []}
        | ToggleParticles ->
            {state with ParticlesAreActive = not state.ParticlesAreActive; Particles = []}
        | FireRocket -> {state with Rockets = Rockets.createRocket state.Ship.Position state.Ship.Heading :: state.Rockets}
        | AspectRatioUpdate newRatio -> {state with AspectRatio = newRatio}
        | TimeUpdate elapsedSeconds-> updateStateWithTime state elapsedSeconds 
        | EndGame -> {state with Running=Stop}
        | NoChange -> state
    UpdateState newState

let processGameAction stateAction action = 
    match action with
    | TriggerRenderFrame -> RenderFrame <| extractState stateAction
    | TriggerUpdateFrame -> UpdateFrame <| extractState stateAction

let processGameEvent (stateAction: StateAction) (gameEvent: GameEvent) = 
    match gameEvent with
    | StateChange change -> updateGameState stateAction change
    | GameAction action -> processGameAction stateAction action



