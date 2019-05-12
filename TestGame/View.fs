module View

open Model
open Update

open System

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open Elmish
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


let inline drawInGameScreen model assets inputs (spriteBatch:SpriteBatch) =
    for ant in model.ants do
        let pos = match ant with | DefaultAnt p | FightingAnt p -> p
        spriteBatch.Draw(assets.whiteTexture, rect pos.X pos.Y 10 10, Color.RosyBrown)
    //for antHild in model.antHill do
    //    spriteBatch.Draw(assets.whiteTexture, rect pos.X pos.Y 10 10, Color.Brown)

let view model dispatch =
    let defaultBorderThickness = 1
    let defaultBorder = border Color.Gray defaultBorderThickness
    let border color = border color defaultBorderThickness
    let defaultHeight = 50
    let background =
        CustomElement(rect 0 0 0 0, fun assets _ spriteBatch ->
            spriteBatch.GraphicsDevice.Clear Color.LightSkyBlue
        )
    match model.gameState with
    | GameIsInMainScreen ->
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
                    (fun _ _ _ -> StartGame |> MainMenuMsg |> dispatch)
                    |> defaultBorder
                   
                button {
                    rectangle = rect 0 0 200 100
                    color = Color.Red
                    content = Some {
                        color = Color.Black
                        text = "Exit"
                        }
                    }
                    (fun _ _ _ -> ExitGame |> MainMenuMsg |> dispatch)
                    |> defaultBorder
            ]
        let listBox = VerticalListBox({ X=0; Y=0}, elements)
        let x = model.screenSize.X / 2 - listBox.Rectangle.Width / 2
        let y = model.screenSize.Y / 2 - listBox.Rectangle.Height / 2
        VerticalListBox({X=x;Y=y}, elements)
        |> createViewableFromTree "font" 20.
    | GameIsRunning inGameModel ->
        let drawGame =
            OnDraw(fun assets input spriteBatch ->
                spriteBatch.End()
                let transformationMatrix =
                    Matrix.CreateTranslation(new Vector3(float32 inGameModel.camera.X,float32 inGameModel.camera.Y, 0.f))
                    |> Nullable.op_Implicit
                spriteBatch.Begin(transformMatrix = transformationMatrix)
                drawInGameScreen inGameModel assets input spriteBatch
            ) |> List.singleton
        let inputHandling =
            [   whilekeydown Input.Keys.W (fun () -> KeyUp |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.S (fun () -> KeyDown |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.A (fun () -> KeyLeft |> UserInteraction |> InGameMsg |> dispatch)
                whilekeydown Input.Keys.D (fun () -> KeyRight |> UserInteraction |> InGameMsg |> dispatch)
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
        let userInterfaceViewable =
            let countElementsHeight = defaultHeight
            let defaultWidth = 200
            let IDK_Height = 300
            horizontalListBox Offset.Zero [
                CustomElement(rect 0 0 0 0, fun c _ spriteBatch ->
                    spriteBatch.End()
                    spriteBatch.Begin()
                    spriteBatch.DrawString(c.fonts.["font"], sprintf "Camera pos %A" inGameModel.camera, Vector2(100.f, 300.f), Color.Black)
                )
                verticalListBox Offset.Zero [
                    horizontalListBox Offset.Zero [
                        verticalListBox Offset.Zero [
                            text {
                                rectangle = rect 0 0 defaultWidth countElementsHeight
                                color = Color.LightGreen
                                content = Some {
                                    color = Color.Black
                                    text = sprintf "Gesamtameisen: %d" inGameModel.ants.Length }
                            } |> defaultBorder
                            text {
                                rectangle = rect 0 0 defaultWidth countElementsHeight
                                color = Color.Green
                                content = Some {
                                    color = Color.Black
                                    text = 
                                        inGameModel.ants
                                        |> List.filter(function | DefaultAnt _ -> true | _ -> false)
                                        |> List.length
                                        |> sprintf "Anzahl Arbeiterameisen: %d" 
                                    }
                            } |> defaultBorder
                            text {
                                rectangle = rect 0 0 defaultWidth countElementsHeight
                                color = Color.Green
                                content = Some {
                                    color = Color.Black
                                    text = "Anzahl Kaempferameisen: XX"
                                    }
                            } |> defaultBorder
                            text {
                                rectangle = rect 0 0 defaultWidth countElementsHeight
                                color = Color.Green
                                content = Some {
                                    color = Color.Black
                                    text = "Anzahl Forscherameisen: XX"
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
                        defaultWidth (model.screenSize.Y - IDK_Height - 4 * countElementsHeight)
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
                    (model.screenSize.X - defaultWidth * 3) defaultHeight
                    Color.Transparent
                text {
                    rectangle = rect 0 0 defaultWidth defaultHeight
                    color = Color.LightGoldenrodYellow
                    content = Some {
                        text = "Forschungspunkte"
                        color = Color.Black }
                } |> defaultBorder
                CustomElement(rect 0 0 0 0, fun c _ spriteBatch ->
                    match inGameModel.selection with
                    | Some(p0, p1) ->
                        let rectangle =
                            let x = min p0.X p1.X
                            let y = min p0.Y p1.Y
                            let width = max p0.X p1.X - x
                            let height = max p0.Y p1.Y - y
                            Rectangle(x, y, width, height)
                        let color = Color(Color.Black, 120)
                        spriteBatch.Draw(c.whiteTexture, rectangle, color)
                    | _ -> ()
                )
                ]
            |> createViewableFromTree "font" 15.
        
        inputHandling
        |> List.append userInterfaceViewable
        |> List.append drawGame

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

