module Domain

open Geometry
open Physics
open OpenTK.Input

type Ship = { 
    Position: Point2d
    BodyWrtOrigin : ColoredTriangle2d
    Heading: float<degree>
    RotationalVelocity : float<degree>
    Velocity: Vector2d 
    Thrust: Acceleration
} 

type GameRunning =
    | Continue
    | Stop

type GameState = {
    Running : GameRunning
    Ship : Ship
}

let initialState = { 
    Running = Continue
    Ship = 
    { 
        Position = {X = 0.0; Y = 0.0;}
        BodyWrtOrigin = 
            {
            P1 ={Color={R=0.2; G=0.9; B=1.0}; Point= {X = 0.0; Y = 0.1;}}; 
            P2 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = -0.1; Y = -0.1;}}; 
            P3 ={Color={R=1.0; G=0.0; B=0.0}; Point= {X = 0.1; Y = -0.1;}};
            }
        Heading = 0.0<degree>
        RotationalVelocity = 0.0<degree>
        Velocity = {Dx = 0.0; Dy = 0.0} 
        Thrust = Neutral 
    }
}

type StateChangeTrigger = 
    | EndGame
    | StartAcceleration of Acceleration
    | StopAcceleration
    | StartHeadingChange of float<degree>
    | StopHeadingChange
    | TimeUpdate
    | NoChange

let transformKeyDown (args: KeyboardKeyEventArgs) =
    match args.Key with
    | Key.Escape ->  StateChangeTrigger.EndGame
    | Key.Up -> StateChangeTrigger.StartAcceleration <| Positive {Dx = 0.0; Dy = 0.002}
    | Key.Down -> StateChangeTrigger.StartAcceleration <| Negative {Dx = 0.002; Dy = 0.002}
    | Key.Right -> StateChangeTrigger.StartHeadingChange 5.0<degree>
    | Key.Left -> StateChangeTrigger.StartHeadingChange -5.0<degree>
    | _ -> StateChangeTrigger.NoChange

let transformKeyUp (args: KeyboardKeyEventArgs) = 
    match args.Key with
    | Key.Up -> StopAcceleration
    | Key.Down -> StopAcceleration 
    | Key.Right -> StateChangeTrigger.StopHeadingChange
    | Key.Left -> StateChangeTrigger.StopHeadingChange
    | _ -> StateChangeTrigger.NoChange

let updateThrust thrustChange (state: GameState) = 
    {state with Ship = {state.Ship with Thrust = thrustChange} }

let updateRotation rotChange state = 
    {state with Ship = {state.Ship with RotationalVelocity =rotChange }}

let updateHeading state  = 
    let constrain value = 
        let upperBoundary = 360.0<degree>
        let lowerBoundary = 0.0<degree>
         //TODO Switch to active pattern
        match value with
        | toLarge when value > upperBoundary -> value - upperBoundary
        | toSmall when value < lowerBoundary -> value + upperBoundary
        | _ -> value  
    let heading = state.Ship.Heading
    {state with Ship = {state.Ship with Heading = constrain <| heading + state.Ship.RotationalVelocity }} 

let updateVelocity state = 
    let vel = state.Ship.Velocity
    let newVel = 
        match state.Ship.Thrust with 
        | Positive acc -> 
            let constrain value = 
                let upperBoundary = 0.05
                let lowerBoundary = -0.05
                max lowerBoundary value |> min upperBoundary
            let thrust = rotate state.Ship.Heading acc
            {Dx = constrain <| vel.Dx + thrust.Dx; Dy = constrain  <| vel.Dy + thrust.Dy}
        | Negative dec -> 
            //TODO Switch to active pattern
            let shrink change value = 
                match value with 
                | nearZero when value > -0.01 && value < 0.01 -> 0.0
                | tooLarge when value > 0.0 -> value - abs change
                | tooSmall when value < 0.0 -> value + abs change
                | _ -> value
            {Dx = shrink dec.Dx vel.Dx ; Dy = shrink dec.Dy vel.Dy}
        | Neutral -> vel
    {state with Ship = {state.Ship with Velocity = newVel}}

let updatePosition state  = 
    let constrain value = 
        let upperBoundary = 2.07
        let lowerBoundary = -upperBoundary
        //TODO Switch to active pattern
        match value with 
        | tooLarge when value > upperBoundary -> lowerBoundary
        | tooSmall when value < lowerBoundary -> upperBoundary
        | _ -> value
    let pos = state.Ship.Position
    let vel = state.Ship.Velocity
    let newPos = {X =  constrain <| pos.X + vel.Dx; Y = constrain <| pos.Y + vel.Dy}
    {state with Ship = {state.Ship with Position = newPos}} 

let updateStateWithTime (oldState: GameState) = 
    let updateIf condition transform value = if condition value then transform value else value

    oldState 
    |> updateIf (fun state -> state.Ship.RotationalVelocity <> 0.0<degree>) updateHeading
    |> updateIf (fun state -> state.Ship.Thrust <> Neutral) updateVelocity
    |> updateIf (fun state -> state.Ship.Velocity <> {Dx = 0.0; Dy = 0.0}) updatePosition

let updateGameState (state: GameState) change = 
    match change with 
    | StartAcceleration thrustChange-> updateThrust thrustChange state
    | StopAcceleration -> updateThrust Neutral state
    | StartHeadingChange rotationChange->  updateRotation rotationChange state
    | StopHeadingChange -> updateRotation 0.0<degree> state
    | TimeUpdate -> updateStateWithTime state
    | EndGame -> {state with Running=Stop}
    | NoChange -> state


