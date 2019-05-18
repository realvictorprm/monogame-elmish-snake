module View

open Model
open Update
open Utils

open System

open Microsoft.Xna.Framework

open Xelmish.Model
open Xelmish.Viewables

open UserInterfaceBase.Model
open UserInterfaceBase
open Microsoft.Xna.Framework.Input

let getAntColor = function
    | WorkerAnt _ -> Color.Red
    | FightingAnt _ -> Color.Green
    | ResearchAnt _ -> Color.Blue

type LineKind =
    | Horizontal of x0:float32 * x1:float32 * y:float32
    | Vertical of y0:float32 * y1:float32 * x:float32

let drawInGameScreen (model:InGameModel) assets inputs (spriteBatch:SpriteBatch) =
    let drawLine lineKind thickness =
        match lineKind with
        | Horizontal(x0, x1, y) ->
            spriteBatch.Draw(
                assets.whiteTexture,
                Vector2(min x0 x1, y),
                Nullable(),
                Color.Gray,
                0.f,
                Vector2.Zero,
                Vector2(abs(x1 - x0), thickness),
                Graphics.SpriteEffects.None,
                0.f)
        | Vertical(y0, y1, x) ->
            spriteBatch.Draw(
                assets.whiteTexture,
                Vector2(x, min y0 y1),
                Nullable(),
                Color.Gray,
                0.f,
                Vector2.Zero,
                Vector2(thickness, abs(y1 - y0)),
                Graphics.SpriteEffects.None,
                0.f)

    let offset =
        Point.Zero
        //((model.camera.position - vec2 worldWidth worldHeight / 2.f) * -1.0f).ToPoint()
    let gridDensity, thickness =
        match model.viewModel.zoomState with
        | Near -> 1, 0.25f
        | Middle -> 3, 0.5f
        | Far -> 8, 1.f
    for x=0 to model.world.Width do
         if x%gridDensity=0 then
           //drawLine(Vertical(0.f - 0.5f, float32 model.world.Height + 0.5f, float32 x - 0.5f))
           drawLine(Vertical(0.f, float32 model.world.Height, float32 x)) thickness
    for y=0 to model.world.Height do
         if y%gridDensity = 0  then
            //drawLine(LineKind.Horizontal(0.f - 0.5f, float32 model.world.Width + 0.5f, float32 y - 0.5f))
            drawLine(LineKind.Horizontal(0.f, float32 model.world.Width, float32 y)) thickness

    for kind in MovingEntity.Cases do
        let currentColor = getAntColor kind
        for (id, _, x, y) in model.world.MovingEntitiesAsListWithPos kind do
            let pos = Point(x, y)
            spriteBatch.Draw(
                assets.whiteTexture,
                pos.ToVector2(),
                Nullable(),
                currentColor,
                0.f,
                Vector2.Zero,
                vec2 1.f 1.f,
                Graphics.SpriteEffects.None,
                0.f)
            
    
    //for antHild in model.antHill do
    //    spriteBatch.Draw(assets.whiteTexture, rect pos.X pos.Y 10 10, Color.Brown)

module GUI =
    let defaultBorderThickness = 1
    let defaultBorder = border Color.Black defaultBorderThickness
    let border color = border color defaultBorderThickness
    let defaultHeight = 50
    let background =
        CustomElement(rect 0 0 0 0, fun _ assets _ spriteBatch ->
            spriteBatch.GraphicsDevice.Clear Color.LightSkyBlue
        )
    
    let mainScreen (screenSize:Point) dispatch =
        let elements =
            [
                background
                button {
                    rectangle = rect 0 0 200 100
                    color = Color.LightCyan
                    content = Some {
                        color = Color.Black
                        text = "Play"
                        }
                    }
                    (fun _ -> StartGame |> MainMenuMsg |> dispatch)
                    |> defaultBorder
                   
                button {
                    rectangle = rect 0 0 200 100
                    color = Color.Red
                    content = Some {
                        color = Color.Black
                        text = "Exit"
                        }
                    }
                    (fun _ -> ExitGame |> MainMenuMsg |> dispatch)
                    |> defaultBorder
            ]
        let listBox = VerticalListBox({ X=0; Y=0}, elements)
        let x = screenSize.X / 2 - listBox.Rectangle.Width / 2
        let y = screenSize.Y / 2 - listBox.Rectangle.Height / 2
        VerticalListBox({X=x;Y=y}, elements)
        |> drawElementTreeWithInput "font" 20.

    let private inGameMap width height inGameModel =
        CustomElement(rect 0 0 width height, fun currentOffset assets _ spriteBatch ->
            spriteBatch.Draw(assets.whiteTexture, Rectangle(currentOffset.ToPoint, Point(width, height)), Color.LightSeaGreen)
            let antHillColor = Color.Yellow
            let worldPosToMapPos pos =
                Vector2(float32 inGameModel.world.Width, float32 inGameModel.world.Height)
                |> Vector2.divide pos
                |> Vector2.multiply (Vector2(width |> float32, height |> float32))
                |> Vector2.add (Vector2(currentOffset.X |> float32, currentOffset.Y |> float32))
            for kind in MovingEntity.Cases do
                for (id, _, x, y) in inGameModel.world.MovingEntitiesAsListWithPos kind do
                    let antPosOnMap = Point(x, y).ToVector2() |> worldPosToMapPos
                    spriteBatch.Draw(assets.whiteTexture, antPosOnMap, getAntColor kind)
                
        ) |> defaultBorder

    let possibleSelectionElement inGameModel =
        CustomElement(rect 0 0 0 0, fun _ assets _ spriteBatch ->
            match inGameModel.viewModel.selection with
            | Some(p0, p1) ->
                let rectangle =
                    let x = min p0.X p1.X
                    let y = min p0.Y p1.Y
                    let width = max p0.X p1.X - x
                    let height = max p0.Y p1.Y - y
                    Rectangle(x, y, width, height)
                let color = Color(Color.Black, 120)
                spriteBatch.Draw(assets.whiteTexture, rectangle, color)
            | _ -> ()
        )

    let inGame (screenSize:Point) inGameModel dispatch =
        let countElementsHeight = defaultHeight
        let defaultWidth = 200
        let IDK_Height = 300
        let opacity = 200
        let applyOpacity (color:Color) =
            Color(color, opacity)
        horizontalListBox Offset.Zero [
            CustomElement(rect 0 0 0 0, fun _ assets _ spriteBatch ->
                spriteBatch.End()
                spriteBatch.Begin()
            )
            verticalListBox Offset.Zero [
                horizontalListBox Offset.Zero [
                    verticalListBox Offset.Zero [
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.LightGreen |> applyOpacity
                            content = Some {
                                color = Color.Black
                                text = sprintf "Gesamtameisen: %d" inGameModel.world.MovingEntityCompleteCount }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green |> applyOpacity
                            content = Some {
                                color = Color.Black
                                text =
                                    MovingEntity(WorkerAnt)
                                    |> inGameModel.world.EntityCount 
                                    |> sprintf "Anzahl Arbeiterameisen: %d" 
                                }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green |> applyOpacity
                            content = Some {
                                color = Color.Black
                                text = 
                                    MovingEntity(FightingAnt)
                                    |> inGameModel.world.EntityCount
                                    |> sprintf "Anzahl Kaempferameisen: %d"
                                }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green |> applyOpacity
                            content = Some {
                                color = Color.Black
                                text =
                                    MovingEntity(ResearchAnt)
                                    |> inGameModel.world.EntityCount
                                    |> sprintf "Anzahl Forscherameisen: %d"
                                }
                        } |> defaultBorder
                    ]
                    text {
                        rectangle = rect 0 0 defaultWidth countElementsHeight
                        color = Color.Purple |> applyOpacity
                        content = Some {
                            color = Color.Black
                            text = "FOOOO: XX" }
                    } |> defaultBorder
                ]
                span 0 0 
                    defaultWidth (screenSize.Y - IDK_Height - defaultBorderThickness * 2 - 4 * (countElementsHeight + defaultBorderThickness * 2))
                    Color.Transparent
                text {
                    rectangle = rect 0 0 IDK_Height IDK_Height
                    color = Color.Yellow |> applyOpacity
                    content = Some {
                        color = Color.Black
                        text = "IDK"
                        }
                } |> defaultBorder
            ]
            span 0 0
                (screenSize.X - (defaultWidth + defaultBorderThickness * 2)* 3) defaultHeight
                Color.Transparent
            verticalListBox Offset.Zero [
                text {
                    rectangle = rect 0 0 defaultWidth defaultHeight
                    color = Color.LightGoldenrodYellow |> applyOpacity
                    content = Some {
                        text = "Forschungspunkte"
                        color = Color.Black }
                } |> defaultBorder
                inGameMap defaultWidth defaultWidth inGameModel
            ]
            possibleSelectionElement inGameModel
            ]
        |> drawElementTreeWithInput "font" 15.

let viewAndInteractions model dispatch =
    match model.gameState with
    | GameIsInMainScreen ->
        let inputHandling =
            [
            ]
        GUI.mainScreen model.screenSize dispatch
        @ inputHandling
        
    | GameIsRunning inGameModel ->
        let drawGame assets input (spriteBatch:SpriteBatch) =
            // In a seperate draw batch the in game world is drawn
            // because the camera translation should only affect the in game world.
                spriteBatch.GraphicsDevice.Clear Color.LightSkyBlue
                let transformationMatrix = 
                    Camera.Transformation inGameModel.camera model.screenSize.X model.screenSize.Y
                    |> Nullable.op_Implicit
                spriteBatch.Begin(transformMatrix = transformationMatrix)
                drawInGameScreen inGameModel assets input spriteBatch
                spriteBatch.End()
        let inputHandling =
            let handleZoom input =
                let diff = input.lastMouseState.ScrollWheelValue - input.mouseState.ScrollWheelValue
                if diff < 0 then
                    CameraZoomIn
                    |> UserInteraction
                    |> InGameMsg
                    |> dispatch
                else if diff > 0 then
                    CameraZoomOut
                    |> UserInteraction
                    |> InGameMsg
                    |> dispatch
            let handleSelection input =
                if input.mouseState.LeftButton = ButtonState.Pressed &&
                    input.lastMouseState.LeftButton = ButtonState.Released then
                    SelectionStarted(Point(input.mouseState.X, input.mouseState.Y))
                    |> InGameMsg
                    |> dispatch
                elif input.lastMouseState.LeftButton = ButtonState.Pressed &&
                    input.mouseState.LeftButton = ButtonState.Released then
                    match inGameModel.viewModel.selection with
                    | Some _ ->
                        SelectionEnded(Point(input.mouseState.X, input.mouseState.Y))
                        |> InGameMsg
                        |> dispatch
                    | None -> ()
                elif input.mouseState.LeftButton = ButtonState.Pressed then
                    match inGameModel.viewModel.selection with
                    | Some _ ->
                        SelectionOngoing(Point(input.mouseState.X, input.mouseState.Y))
                        |> InGameMsg
                        |> dispatch
                    | None -> ()
            [   whilekeydown Input.Keys.W (fun () -> CameraUp |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.S (fun () -> CameraDown |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.A (fun () -> CameraLeft |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.D (fun () -> CameraRight |> UserInteraction |> InGameMsg |> dispatch)
                onkeydown Input.Keys.Escape (fun () -> DoExitGame |> UserInteraction |> InGameMsg |> dispatch)
                OnUpdate(fun input ->
                    handleZoom input
                    handleSelection input
                )]
        
        [
            // If the game is in the InGameState, the state should be recalculated
            // each game update (16ms in theory). Moreover for animations we're
            // particulary interested in the current game time.
            yield
                OnUpdate(fun input ->
                    InGameMsg.UpdateTick input.gameTime
                    |> InGameMsg
                    |> dispatch)
            // Check the input handling
            yield! inputHandling
            yield
                OnDraw(fun assets input spriteBatch ->
                    // Draw the world
                    drawGame assets input spriteBatch
                )
            yield!
                // Draw the GUI
                GUI.inGame model.screenSize inGameModel dispatch
        ]

    | GameOver ->
        let inputs =
            [
                onkeydown Input.Keys.Escape (fun () -> exit 0)
            ]
        horizontalListBox Offset.Zero
            [
                text {
                    rectangle = rect 0 0 100 50
                    color = Color.Transparent
                    content = Some { 
                        color = Color.Black
                        text = "Game over"
                    }
                }
            ]
        |> drawElementTreeWithInput "font" 15.
        |> List.append inputs

