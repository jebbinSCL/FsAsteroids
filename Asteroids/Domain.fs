module Domain

open Geometry
open Physics
open Ship
open Particles
open Asteroids

type GameRunning =
    | Continue
    | Stop   

type GameState = {
    Running : GameRunning
    AspectRatio : float
    TrailIsActive : bool
    ParticlesAreActive : bool
    Ship : Ship
    Asteroids : Asteroid list
    Shards : Asteroid list
    TrailParticles : Particle list
    Particles : Particle list
    Rockets : Particle list
}

type StateChangeTrigger = 
    | EndGame
    | ChangeAcceleration of Acceleration
    | ChangeHeading of float<degree>
    | ToggleTrail
    | ToggleParticles
    | FireRocket
    | TestAsteroidBreak
    | TestAsteroidShatter
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

let initialGameState aspectratio = { 
    Running = Continue
    AspectRatio = 1.0
    TrailIsActive = false
    ParticlesAreActive = false
    Ship = Ship.initialShip
    Asteroids = createInitialAsteroids aspectratio
    Shards = []
    TrailParticles = []
    Particles = []
    Rockets = []
}



