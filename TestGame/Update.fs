module Update
open Model
open Utils

open Elmish

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let random = new System.Random()

let worldWidth = 1000.f
let worldHeight = 1000.f

let cameraInWorld (vector:Vector2) =
    vector.X >= 0.f && vector.Y >= 0.f &&
        vector.X <= worldWidth &&
        vector.Y <= worldHeight

let update msg model : Model.Model * Cmd<Msg> =
    match model.gameState with
    | GameIsRunning model ->
        match msg with
        | InGameMsg(UserInteraction key) ->
            let cameraTranslation =
                match key with
                | Esc -> exit 0
                | KeyUp -> Vector2(0.f, 10.f)
                | KeyDown -> Vector2(0.f, -10.f)
                | KeyLeft -> Vector2(10.f, 0.f)
                | KeyRight -> Vector2(-10.f, 0.f)
            let newPos = 
                match Vector2.Add(model.camera, cameraTranslation) with
                | newPos when cameraInWorld(newPos) -> newPos
                | _ -> model.camera

            { model with camera = newPos }
            |> GameIsRunning, Cmd.none
        | InGameMsg(SelectionStarted pos) ->
            { model with selection = (pos, pos) |> Some }
            |> GameIsRunning, Cmd.none
        | InGameMsg(SelectionOngoing endPos) ->
            match model.selection with 
            | Some(startPos, _) ->
                { model with selection = (startPos, endPos)|> Some }
                |> GameIsRunning, Cmd.none
            | None -> GameIsRunning model, Cmd.none
        | InGameMsg(SelectionEnded endPos) ->
            // Good point to handle finding out which entities lie within the selection
            match model.selection with
            | Some(startPos, _) ->
                // { model with selection = (startPos, endPos)|> Some }
                { model with selection = None }
                |> GameIsRunning, Cmd.none
            | None -> GameIsRunning model, Cmd.none
        | TimeElapsed
        | MainMenuMsg _ ->
            GameIsRunning model, Cmd.none
    | GameOver -> GameOver, Cmd.none
    | GameIsInMainScreen -> 
        match msg with
        | MainMenuMsg StartGame ->
            model.assets.sounds.["bang"].Play() |> ignore
            GameIsRunning(InGameModel.Empty), Cmd.none
        | MainMenuMsg ExitGame ->
            exit 0, Cmd.none
        | _ -> GameIsInMainScreen, Cmd.none
    |> (fun (state, cmd) -> { model with gameState = state }, cmd)

let initialModel = GameIsInMainScreen

let init screenSize loadedAssets : Model.Model * Cmd<_> =
    { gameState = initialModel; assets = loadedAssets; screenSize=screenSize}, Cmd.none

let timerSubscription dispatch =
    async {
        while true do
            do! Async.Sleep 250
            TimeElapsed |> dispatch

    } |> Async.Start

let subscriptions = [
        timerSubscription
    ]