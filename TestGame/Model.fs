module Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio
open System
open Microsoft.Xna.Framework.Content

[<Struct>]
type Position = {
    X:int
    Y:int
}

[<Struct>]
type Direction =
    | Up
    | Down
    | Left
    | Right

[<NoComparison>]
type StaticEntity =
| Stone of stonePos:Point
| RiverPart of riverPos:Point
| Bridge of bridgePos:Point
| Food of foodPos:Point
| AntHill of antHilPos:Point

[<NoComparison>]
type Ant =
| DefaultAnt of Point
| FightingAnt of Point

[<NoComparison>]
type MovingEntity = | MovingEntity of Ant

type Selection = Point * Point

type InGameModel = {
    ants:Ant list
    antHills: Position list
    camera: Vector2
    selection: Selection option
} with
    static member Empty = {
        ants = [ for i in 0..100 -> DefaultAnt(Point(-i * 10, -i * 10)) ]
        antHills = [ ]
        camera = Vector2.Zero
        selection = None
    }


type GameState =
    | GameOver
    | GameIsInMainScreen
    | GameIsRunning of InGameModel

[<Struct>]
type UserInteraction =
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | Esc
    
type Collision =
    | Wall
    | Food of newFoodPosition:Position
    | Tail of remainingSnakeParts:Position list

[<Struct>]
type MainMenuMsg =
    | StartGame
    | ExitGame

[<Struct>]
type InGameMessage =
| AntsSelected
| AntsCreated
| AntTypeSelected
| AntTargetChosen
| AutomaticBehaviourSelected
| AntSelectFood
| AntGoResearch
| HillUpgradeSelected
| UpdateTick of GameTime * Input.KeyboardState * MouseState
| ExitPressed
| RedrawRequested
| ColorChanged of Color

type Actions =
// Collision detection / In range detection
| AntInFight
// Could be a performance problem
| AntDying

[<Struct>]
type InGameMsg =
    | UserInteraction of UserInteraction
    | SelectionStarted of start:Point
    | SelectionOngoing of corner:Point
    | SelectionEnded of pEnd:Point

[<Struct>]
type Msg = 
    | MainMenuMsg of mainMenu:MainMenuMsg
    | InGameMsg of inGame:InGameMsg
    | TimeElapsed

[<Struct>]
type Model = {
    mutable gameState: GameState
    assets: Xelmish.Model.LoadedAssets
    screenSize: Point
}
