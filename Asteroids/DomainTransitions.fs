module DomainTransitions

open Domain

let extractState (action : StateAction) = 
    match action with 
    | UpdateState state | RenderFrame state | UpdateFrame state -> state

let updateShipHeading state = {state with Ship = Ship.updateHeading state.Ship}

let updateShipVelocity state = {state with Ship = Ship.updateVelocity state.Ship}

let updateShipPosition (aspectRatio : float) state = {state with Ship = Ship.updatePosition aspectRatio state.Ship} 

let updateTrail elapsed (aspectRatio : float) (state: GameState) = 
    let trail = TrailParticles.updateParticles state.TrailParticles elapsed aspectRatio state.Ship.Position state.Ship.Thrust state.Ship.Heading
    {state with TrailParticles = trail} 

let updateParticles elapsed (aspectRatio : float) (state: GameState) = 
    let particles = Particles.updateParticles state.Particles elapsed aspectRatio state.Ship.Position state.Ship.Thrust state.Ship.Heading
    {state with Particles = particles} 

let updateRockets elapsed (aspectRatio : float) (state: GameState) = 
    let rockets = Rockets.updateRockets elapsed aspectRatio state.Rockets
    {state with Rockets = rockets}

let updateAsteroids elapsed (aspectRatio : float) (state: GameState) = 
    let asteroids = Asteroids.updateAsteroids elapsed aspectRatio state.Asteroids
    {state with Asteroids = asteroids}

let updateShards elapsed (aspectRatio : float) (state: GameState) = 
    let asteroids = Asteroids.updateShards elapsed aspectRatio state.Shards
    {state with Shards = asteroids}

let updateStateWithTime elapsed (oldState: GameState)  = 
    let updateIf condition transform value = if condition value then transform value else value
    oldState 
    |> updateIf (fun state -> state.Ship.RotationalVelocity <> Physics.neutralRotationalVelocity) updateShipHeading
    |> updateIf (fun state -> state.Ship.Thrust <> Physics.neutralAcceleration) updateShipVelocity
    |> updateIf (fun state -> state.Ship.Velocity <> Physics.neutralVelocity) (updateShipPosition oldState.AspectRatio)
    |> updateIf (fun state -> state.ParticlesAreActive) (updateParticles elapsed oldState.AspectRatio)
    |> updateIf (fun state -> state.TrailIsActive) (updateTrail elapsed oldState.AspectRatio)
    |> updateRockets elapsed oldState.AspectRatio
    |> updateAsteroids elapsed oldState.AspectRatio
    |> updateShards elapsed oldState.AspectRatio
    |> Collisions.detectCollisions

let updateGameState stateAction change = 
    let state = extractState stateAction
    let newState = 
        match change with 
        | ChangeAcceleration newThrust-> 
            {state with Ship = Ship.updateThrust newThrust state.Ship }
        | ChangeHeading newRotationalVelocity->  
            {state with Ship = Ship.updateRotationalVelocity newRotationalVelocity state.Ship }
        | ToggleTrail ->
            {state with TrailIsActive = not state.TrailIsActive; TrailParticles = []}
        | ToggleParticles ->
            {state with ParticlesAreActive = not state.ParticlesAreActive; Particles = []}
        | FireRocket -> {state with Rockets = Rockets.createRocket state.Ship.Position state.Ship.Heading :: state.Rockets}
        | TestAsteroidBreak -> {state with Asteroids = Asteroids.breakAsteroids state.AspectRatio state.Asteroids}
        | TestAsteroidShatter -> {state with Shards = Asteroids.shatterAsteroids state.AspectRatio state.Asteroids; Asteroids = []}
        | AspectRatioUpdate newRatio -> {state with AspectRatio = newRatio}
        | TimeUpdate elapsedSeconds-> updateStateWithTime elapsedSeconds state  
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