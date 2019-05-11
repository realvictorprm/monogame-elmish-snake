module Update
open Model
open Utils

open Elmish

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let random = new System.Random()

let worldWidth = 20
let worldHeight = 20

let tryGetCollision model =
    let head = model.snakeParts |> List.head
    // Yummy food!
    if head.X = model.food.X &&
        head.Y = model.food.Y then
        { X = random.Next(worldWidth)
          Y = random.Next(worldHeight)}
        |> Collision.Food
        |> Some
    // We're biting us!
    else
        let possibleBitingPoint =
            model.snakeParts
            |> List.tail
            |> List.tryFindIndex(fun e -> e.X = head.X && e.Y = head.Y)
        match possibleBitingPoint with
        | Some index ->
            model.snakeParts
            |> List.take index
            |> Collision.Tail
            |> Some
        | None ->
            if head.X < 0 ||
                head.Y < 0 ||
                head.Y >= worldHeight ||
                head.X >= worldWidth then
                Collision.Wall
                |> Some
            else
                None

let updateGameIsRunning msg model =
    match msg with
    | Input userInteraction ->
        let isValidNewDirection oldDirection newDirection =
            match oldDirection, newDirection with
            | Up, Down
            | Down, Up
            | Right, Left
            | Left, Right -> false
            | _ -> true
        let newDirection =
            match userInteraction with
            | KeyUp -> Up
            | KeyDown -> Down
            | KeyLeft -> Left
            | KeyRight -> Right
            | Esc -> exit 0
        if isValidNewDirection model.oldDirection newDirection then
            { model with direction = newDirection }
        else
            model
        |> GameIsRunning, Cmd.none
    | TimeElapsed ->
        let currentDirection = model.direction
        let oldDirection = model.direction
        let newSnakePartsPositions =
            let remainingParts =
                model.snakeParts
                |> List.rev
                |> List.tail
                |> List.rev
            let xPlus, yPlus =
                currentDirection
                |> getTranslationFromDirection
            let oldHead =
                model.snakeParts
                |> List.head
            let newHead = {
                X = oldHead.X + xPlus
                Y = oldHead.Y + yPlus
            }
            newHead :: remainingParts
        // Collision detection
        let preCollisionModel =
            { model with
                snakeParts = newSnakePartsPositions
                direction = currentDirection
                oldDirection = currentDirection }
        match tryGetCollision preCollisionModel with
        | Some collision ->
            match collision with
            | Wall -> GameOver, Cmd.none
            | Food newFoodPosition ->
                let lastElement =
                    let defaultLastElement () =
                        let newHead =
                            preCollisionModel.snakeParts
                            |> List.head
                        let xPlus, yPlus =
                            currentDirection
                            |> getInvertedDirection 
                            |> getTranslationFromDirection
                        {   X = newHead.X + xPlus
                            Y = newHead.Y + yPlus }
                    model.snakeParts
                    |> List.rev
                    |> List.tryHead
                    |> (function | Some a -> a | None -> defaultLastElement ())
                { preCollisionModel with
                    snakeParts = [lastElement] |> List.append preCollisionModel.snakeParts
                    food = newFoodPosition }
                |> GameIsRunning, Cmd.none
            | Tail remainingSnakeParts ->
                { preCollisionModel with 
                    snakeParts = remainingSnakeParts }
                |> GameIsRunning, Cmd.none
        | None ->
            preCollisionModel
            |> GameIsRunning, Cmd.none

let update msg gameState : Model.Model * Cmd<Msg> =
    match gameState with
    | GameIsRunning(model) -> updateGameIsRunning msg model
    | GameOver -> GameOver, Cmd.none

let initialModel =
    { snakeParts = [for i in 0..5 -> {X = 5; Y = 5 + i}]
      direction = Right
      food = {X = 10; Y = 10}
      oldDirection = Up }
    |> GameIsRunning

let init () : GameState * Cmd<_> =
    initialModel, Cmd.none

let timerSubscription dispatch =
    async {
        while true do
            do! Async.Sleep 250
            TimeElapsed |> dispatch

    } |> Async.Start

let subscriptions = [
        timerSubscription
    ]