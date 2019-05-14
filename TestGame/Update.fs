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

let updateInGameModel msg (model:InGameModel) : Model.GameState * Cmd<Msg> =
    match msg with
    | UserInteraction key ->
        let cameraTranslation =
            match key with
            | Esc -> exit 0
            | KeyUp -> Vector2(0.f, -10.f)
            | KeyDown -> Vector2(0.f, 10.f)
            | KeyLeft -> Vector2(-10.f, 0.f)
            | KeyRight -> Vector2(10.f, 0.f)
        let newCamera = 
            match model.camera.Translate cameraTranslation with
            | newCamera when cameraInWorld(newCamera.position) -> newCamera
            | _ -> model.camera

        { model with camera = newCamera }
        |> GameIsRunning, Cmd.none
    | SelectionStarted pos ->
        { model with selection = (pos, pos) |> Some }
        |> GameIsRunning, Cmd.none
    | SelectionOngoing endPos ->
        match model.selection with 
        | Some(startPos, _) ->
            { model with selection = (startPos, endPos)|> Some }
            |> GameIsRunning, Cmd.none
        | None -> GameIsRunning model, Cmd.none
    | SelectionEnded endPos ->
        // Good point to handle finding out which entities lie within the selection
        match model.selection with
        | Some(startPos, _) ->
            // { model with selection = (startPos, endPos)|> Some }
            { model with selection = None }
            |> GameIsRunning, Cmd.none
        | None -> GameIsRunning model, Cmd.none
    | UpdateTick currentGameTime ->
        
        model |> GameIsRunning, Cmd.none

let updateMainMenu msg (model:Model.Model) =
    match msg with
    | StartGame ->
        model.assets.sounds.["bang"].Play() |> ignore
        GameIsRunning({
            InGameModel.Empty with
                camera = {
                    position = Vector2(worldWidth / 2.f, worldHeight / 2.f)
                    bounds = Rectangle(0, 0, worldWidth |> int, worldHeight |> int)
                }
        }), Cmd.none
    | ExitGame ->
        exit 0, Cmd.none

let update msg model : Model.Model * Cmd<Msg> =
    match model.gameState with
    | GameIsRunning innerModel ->
        match msg with
        | InGameMsg innerMsg ->
            updateInGameModel innerMsg innerModel
        | MainMenuMsg _ ->
            model.gameState, Cmd.none
    | GameOver ->
        GameOver, Cmd.none
    | GameIsInMainScreen -> 
        match msg with
        | MainMenuMsg innerMsg ->
            updateMainMenu innerMsg model
        | _ -> model.gameState, Cmd.none
    |> (fun (state, cmd) -> { model with gameState = state }, cmd)

let initialModel = GameIsInMainScreen

let init screenSize loadedAssets : Model.Model * Cmd<_> =
    { gameState = initialModel; assets = loadedAssets; screenSize=screenSize}, Cmd.none

//let timerSubscription dispatch =
//    async {
//        while true do
//            do! Async.Sleep 250
//            TimeElapsed |> dispatch

//    } |> Async.Start

let subscriptions = [
        // timerSubscription
    ]