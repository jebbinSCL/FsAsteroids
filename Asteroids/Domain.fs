module Domain

open Geometry
open Physics
open Ship

type GameRunning =
    | Continue
    | Stop

type ParticleSystemState = 
    | On
    | Off

type GameState = {
    Running : GameRunning
    ParticleSystemState : ParticleSystemState
    Ship : Ship
}

let initialState = { 
    Running = Continue
    ParticleSystemState = Off
    Ship = Ship.initialShip
}

type StateChangeTrigger = 
    | EndGame
    | StartAcceleration of Acceleration
    | StopAcceleration
    | StartHeadingChange of float<degree>
    | StopHeadingChange
    | ToggleParticles
    | TimeUpdate
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

let updatePosition state = {state with Ship = Ship.updatePosition state.Ship} 

let updateStateWithTime (oldState: GameState) = 
    let updateIf condition transform value = if condition value then transform value else value

    oldState 
    |> updateIf (fun state -> state.Ship.RotationalVelocity <> Ship.neutralRotationalVelocity) updateHeading
    |> updateIf (fun state -> state.Ship.Thrust <> Ship.neutralThrust) updateVelocity
    |> updateIf (fun state -> state.Ship.Velocity <> Ship.neutralVelocity) updatePosition

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
        | ToggleParticles ->
            let newParticleState =  match state.ParticleSystemState with | On -> Off | Off -> On
            {state with ParticleSystemState =newParticleState}
        
        | TimeUpdate -> updateStateWithTime state
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



