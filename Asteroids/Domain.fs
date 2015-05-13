module Domain

open Geometry
      
type Ship = { 
    Position: Point
    Velocity: Vector 
}

type GameRunning =
    | Continue
    | Stop

type GameState = {
    Running : GameRunning
    Ship : Ship
}

//For state changes based on user events. 
//Probably a good idea to have a seperate union for state changes base on internal game events
type UserStateChange = 
    | EndGame
    | ChangePosition of Point 
    | NoChange

let initialState = { 
    Running = Continue
    Ship = { Position = {X = 0.0; Y = 0.0;}; Velocity = {Dx = 0.0; Dy = 0.0} }
}