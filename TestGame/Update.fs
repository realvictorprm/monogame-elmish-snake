module Update
open Model
open Utils

open Elmish

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let random = new System.Random()

let cameraInWorld (vector:Vector2) worldWidth worldHeight =
    vector.X >= 0.f && vector.Y >= 0.f &&
        vector.X <= float32 worldWidth &&
        vector.Y <= float32 worldHeight

let updateInGameModel msg (model:InGameModel) : Model.GameState * Cmd<Msg> =
    match msg with
    | UserInteraction key ->
        match key with
        | DoExitGame -> exit 0
        | CameraZoomIn ->
            //let zoom = if model.camera.zoom > 2. then model.camera.zoom 
            let newZoomState =
                match model.viewModel.zoomState with
                | Near -> Near
                | Middle -> Near
                | Far -> Middle
            let newCamera = { model.camera with zoom = Zoom.toValue newZoomState |> float32 }
            { model with camera = newCamera; viewModel = { model.viewModel with zoomState = newZoomState }}
        | CameraZoomOut ->
            let newZoomState =
                match model.viewModel.zoomState with
                | Near -> Middle
                | Middle -> Far
                | Far -> Far
            let newCamera = { model.camera with zoom = Zoom.toValue newZoomState |> float32 }
            { model with camera = newCamera; viewModel = { model.viewModel with zoomState = newZoomState }}
        | CameraUp -> 
            let cameraTranslation = Vector2(0.f, -10.f)
            let newCamera = 
                match model.camera.Translate cameraTranslation with
                | newCamera when cameraInWorld(newCamera.position) model.world.Width model.world.Height -> newCamera
                | _ -> model.camera
            { model with camera = newCamera }
        | CameraDown -> 
            let cameraTranslation = Vector2(0.f, 10.f)
            let newCamera = 
                match model.camera.Translate cameraTranslation with
                | newCamera when cameraInWorld(newCamera.position) model.world.Width model.world.Height -> newCamera
                | _ -> model.camera
            { model with camera = newCamera }
        | CameraLeft -> 
            let cameraTranslation = Vector2(-10.f, 0.f)
            let newCamera = 
                match model.camera.Translate cameraTranslation with
                | newCamera when cameraInWorld(newCamera.position) model.world.Width model.world.Height -> newCamera
                | _ -> model.camera
            { model with camera = newCamera }
        | CameraRight -> 
            let cameraTranslation = Vector2(10.f, 0.f)
            let newCamera = 
                match model.camera.Translate cameraTranslation with
                | newCamera when cameraInWorld(newCamera.position) model.world.Width model.world.Height -> newCamera
                | _ -> model.camera
            { model with camera = newCamera }
        |> GameIsRunning, Cmd.none
    | SelectionStarted pos ->
        { model with viewModel = { model.viewModel with selection = (pos, pos) |> Some }}
        |> GameIsRunning, Cmd.none
    | SelectionOngoing endPos ->
        match model.viewModel.selection with 
        | Some(startPos, _) ->
            { model with viewModel = { model.viewModel with selection = (startPos, endPos)|> Some }}
            |> GameIsRunning, Cmd.none
        | None -> GameIsRunning model, Cmd.none
    | SelectionEnded endPos ->
        // Good point to handle finding out which entities lie within the selection
        match model.viewModel.selection with
        | Some(startPos, _) ->
            // { model with selection = (startPos, endPos)|> Some }
            { model with viewModel = { model.viewModel with selection = None }}
            |> GameIsRunning, Cmd.none
        | None -> GameIsRunning model, Cmd.none
    | UpdateTick currentGameTime ->
        let newCounter = (model.counter + 1) % 21
        if newCounter = 20 then
            for entity, id, vector in model.movingEntities |> Array.take 20  do
                let (x, y) =
                    model.world.TryGetPosOf(entity |> MovingEntity, id)
                    |> Option.defaultValue (10, 10)
                if model.world.MoveEntity(entity |> MovingEntity, id, x + (vector.X |> int32), y) then
                    model.world.MoveEntity(entity |> MovingEntity, id, x + (vector.X |> int32), y + (vector.Y |> int32))
                    |> ignore
                else if model.world.MoveEntity(entity |> MovingEntity, id, x, y + (vector.Y |> int32)) then
                    model.world.MoveEntity(entity |> MovingEntity, id, x + (vector.X |> int32), y + (vector.Y |> int32))
                    |> ignore
        { model with counter = newCounter } |> GameIsRunning, Cmd.none

let updateMainMenu msg (model:Model.Model) =
    match msg with
    | StartGame ->
        model.assets.sounds.["bang"].Play() |> ignore
        let worldSize =
            Vector2(
                float32 InGameModel.Empty.world.Width,
                float32 InGameModel.Empty.world.Height)
        let inGameModel =
            {InGameModel.Empty with
                camera = {
                    position = 
                        worldSize * 0.5f
                    zoom = 10.0f
                    bounds = Rectangle(0, 0, worldSize.X |> int, worldSize.Y |> int)
                }
            }
        let maxI = 100 - 1
        let factor = 10
        let movingEntities =
            [|
                for i in 0..maxI do
                    match inGameModel.world.AddEntity(WorkerAnt|> MovingEntity, i * factor, i * factor, MovingEntityData()) with
                    | Some id ->
                        yield
                            WorkerAnt,
                            id,
                            Vector2(1.0f, 1.0f)
                    | _ -> ()
            |]
        for i in 0..maxI do
            inGameModel.world.AddEntity(FightingAnt |> MovingEntity, i * factor, maxI * factor - i * factor, MovingEntityData())
            |> ignore
        GameIsRunning({ inGameModel with movingEntities = movingEntities}), Cmd.none
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