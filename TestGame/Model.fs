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
    
type InGameModel = {
    snakeParts: Position list
    food: Position
    direction: Direction
    oldDirection: Direction
}
type GameState =
    | GameOver
    | GameIsRunning of InGameModel

[<Struct>]
type UserInteraction =
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    
type Collision =
    | Wall
    | Food of newFoodPosition:Position
    | Tail of remainingSnakeParts:Position list
    
[<Struct>]
type Msg = 
    | Input of userInteraction:UserInteraction
    | TimeElapsed
    // Messages from the main framework which cannot be changed / at least shouldn't
    | RedrawRequested
    | UpdateTick of time:GameTime * KeyboardState * MouseState

type Model = GameState

[<Struct>]
[<NoComparison>]
type Content = {
    spriteBatch: SpriteBatch
    font: SpriteFont
    rect: Texture2D
} with 
    interface IDisposable with
        override self.Dispose() =
            self.spriteBatch.Dispose()
            self.rect.Dispose()
