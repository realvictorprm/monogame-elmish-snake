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


(*

| GameIsRunning ->
    let elements =
        let borderThickness = 1
        [
            Border(Color.Gray, borderThickness,
                Button({
                    rectangle = rect 0 0 200 50
                    color = Color.Green
                    content = Some {
                        color = Color.Black
                        text = "Gesamtameisen: XX"
                        }
                    }, 
                    fun _ _ _ -> ()
                )
            )
            Border(Color.Gray, borderThickness,
                Button({
                    rectangle = rect 0 0 200 50
                    color = Color.DarkGreen
                    content = Some {
                        color = Color.Black
                        text = "Anzahl Arbeiterameisen: XX"
                        }
                    },
                    fun _ _ _ -> ()
                )
            )
            Border(Color.Gray, borderThickness,
                Button({
                    rectangle = rect 0 0 200 50
                    color = Color.DarkGreen
                    content = Some {
                        color = Color.Black
                        text = "Anzahl Kaempferameisen: XX"
                        }
                    },
                    fun _ _ _ -> ()
                )
            )
            Border(Color.Gray, borderThickness,
                Button({
                    rectangle = rect 0 0 200 50
                    color = Color.DarkGreen
                    content = Some {
                        color = Color.Black
                        text = "Anzahl Forscherameisen: XX"
                        }
                    },
                    fun _ _ _ -> ()
                )
            )
        ]
    Border(Color.Black,
        2,
        VerticalListBox({ X=0; Y=0}, elements)
    )
    |> Utils.View.createViewableFromTree "font" 15.
    |> List.append [
        onkeydown Input.Keys.Escape (fun () -> exit 0)
    ]
*)

let getAntColor = function
    | WorkerAnt _ -> Color.Brown
    | FightingAnt _ -> Color.Red
    | ResearchAnt _ -> Color.Blue


let drawInGameScreen model assets inputs (spriteBatch:SpriteBatch) =
    
    for kind in MovingEntity.Cases do
        let currentColor = getAntColor kind
        for (id, pos) in model.world.MovingEntitiesAsList kind do
            spriteBatch.Draw(assets.whiteTexture,
                Rectangle((pos.ToVector2() - (model.camera.position - vec2 worldWidth worldHeight / 2.f)).ToPoint(),
                    Point(20, 20)
                ),
                currentColor)
            
    
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
        |> createViewableFromTree "font" 20.

    let private inGameMap width height inGameModel =
        CustomElement(rect 0 0 width height, fun currentOffset assets _ spriteBatch ->
            spriteBatch.Draw(assets.whiteTexture, Rectangle(currentOffset.ToPoint, Point(width, height)), Color.Green)
            let antHillColor = Color.Yellow
            let worldPosToMapPos pos =
                vec2 worldWidth worldHeight
                |> Vector2.divide pos
                |> Vector2.multiply (Vector2(width |> float32, height |> float32))
                |> Vector2.add (Vector2(currentOffset.X |> float32, currentOffset.Y |> float32))
            for kind in MovingEntity.Cases do
                for (id, pos) in inGameModel.world.MovingEntitiesAsList kind do
                    let antPosOnMap = pos.ToVector2() |> worldPosToMapPos
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
                            color = Color.LightGreen
                            content = Some {
                                color = Color.Black
                                text = sprintf "Gesamtameisen: %d" inGameModel.world.MovingEntityCompleteCount }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green
                            content = Some {
                                color = Color.Black
                                text =
                                    inGameModel.world.MovingEntityCount WorkerAnt
                                    |> sprintf "Anzahl Arbeiterameisen: %d" 
                                }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green
                            content = Some {
                                color = Color.Black
                                text = 
                                    inGameModel.world.MovingEntityCount FightingAnt
                                    |> sprintf "Anzahl Kaempferameisen: %d"
                                }
                        } |> defaultBorder
                        text {
                            rectangle = rect 0 0 defaultWidth countElementsHeight
                            color = Color.Green
                            content = Some {
                                color = Color.Black
                                text = 
                                    inGameModel.world.MovingEntityCount ResearchAnt
                                    |> sprintf "Anzahl Forscherameisen: %d"
                                }
                        } |> defaultBorder
                    ]
                    text {
                        rectangle = rect 0 0 defaultWidth countElementsHeight
                        color = Color.Purple
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
                    color = Color.Yellow
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
                    color = Color.LightGoldenrodYellow
                    content = Some {
                        text = "Forschungspunkte"
                        color = Color.Black }
                } |> defaultBorder
                inGameMap defaultWidth defaultWidth inGameModel
            ]
            possibleSelectionElement inGameModel
            ]
        |> createViewableFromTree "font" 15.

let viewAndInteractions model dispatch =
    match model.gameState with
    | GameIsInMainScreen ->
        let inputHandling =
            [
            ]
        GUI.mainScreen model.screenSize dispatch
        @ inputHandling
        
    | GameIsRunning inGameModel ->
        let drawGame =
            // In a seperate draw batch the in game world is drawn
            // because the camera translation should only affect the in game world.
            OnDraw(fun assets input spriteBatch ->
                spriteBatch.End()
                let transformationMatrix = 
                    Matrix.Identity
                    //Matrix.Identity *
                    //Matrix.CreateTranslation(-float32 inGameModel.camera.position.X, -float32 inGameModel.camera.position.Y, 0.f)
                    |> Nullable.op_Implicit
                // spriteBatch.Begin(transformMatrix = transformationMatrix)
                spriteBatch.Begin()
                drawInGameScreen inGameModel assets input spriteBatch
                spriteBatch.DrawString(assets.fonts.["font"], sprintf "Camera pos %A" inGameModel.camera, Vector2(100.f, 300.f), Color.Black)
            )
        let inputHandling =
            [   whilekeydown Input.Keys.W (fun () -> CameraUp |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.S (fun () -> CameraDown |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.A (fun () -> CameraLeft |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.D (fun () -> CameraRight |> UserInteraction |> InGameMsg |> dispatch)
                OnUpdate(fun input ->
                    if input.mouseState.LeftButton = ButtonState.Pressed &&
                        input.lastMouseState.LeftButton = ButtonState.Released then
                        SelectionStarted(Point(input.mouseState.X, input.mouseState.Y))
                        |> InGameMsg
                        |> dispatch
                    elif input.lastMouseState.LeftButton = ButtonState.Pressed &&
                        input.mouseState.LeftButton = ButtonState.Released then
                        match inGameModel.selection with
                        | Some _ ->
                            SelectionEnded(Point(input.mouseState.X, input.mouseState.Y))
                            |> InGameMsg
                            |> dispatch
                        | None -> ()
                    elif input.mouseState.LeftButton = ButtonState.Pressed then
                        match inGameModel.selection with
                        | Some _ ->
                            SelectionOngoing(Point(input.mouseState.X, input.mouseState.Y))
                            |> InGameMsg
                            |> dispatch
                        | None -> ()
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
            // Draw the world
            yield drawGame
            // Draw the GUI
            yield! GUI.inGame model.screenSize inGameModel dispatch
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
        |> createViewableFromTree "font" 15.
        |> List.append inputs

