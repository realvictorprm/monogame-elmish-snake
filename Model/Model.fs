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
[<Struct>]
type Ant =
| DefaultAnt of d:Point
| FightingAnt of f:Point
with
    member self.Position =
        match self with
        | DefaultAnt pos
        | FightingAnt pos -> pos

[<NoComparison>]
type MovingEntity = | MovingEntity of Ant

type Selection = Point * Point

type Camera = {
    position: Vector2
    bounds: Rectangle
} with
    member self.Translate vec =
        { self with position = Vector2.Add(vec, self.position) }

    static member Default =
        { position = Vector2.Zero 
          bounds = Rectangle(0, 0, 100, 100) }

[<Struct>]
type InGameModel = {
    ants:Ant list
    antHills: Position list
    camera: Camera
    selection: Selection option
} with
    static member Empty = {
        ants =
            [ for x in 0..99 do
                for y in 0..99 do
                    yield DefaultAnt(Point(x * 10, y * 10)) ]
        antHills = [ ]
        camera = Camera.Default
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
type UpdateTickMsg = {
    gameTime:GameTime
    keyboardState:Input.KeyboardState
    mouseState:MouseState 
}

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
| UpdateTick of updateTick:UpdateTickMsg
| ExitPressed
| RedrawRequested
| ColorChanged of colorChanged:struct {| colorChangedColor:Color |}

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
    | UpdateTick of gameTime:GameTime

[<Struct>]
type Msg = 
    | MainMenuMsg of mainMenu:MainMenuMsg
    | InGameMsg of inGame:InGameMsg

[<Struct>]
type Model = {
    mutable gameState: GameState
    assets: Xelmish.Model.LoadedAssets
    screenSize: Point
}
