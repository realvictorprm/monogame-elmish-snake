﻿module UserInterfaceBase
open Xelmish.Viewables
open Xelmish.Model

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module Model =

    type BorderStyle = Color
    type TickFun = LoadedAssets -> Inputs -> SpriteBatch -> unit

    [<Struct>]
    type Offset = {
        X:int
        Y:int
    } with
        static member Zero = {X=0;Y=0}

    [<Struct>]
    type TextContent = {
        text: string
        color: Color
    }

    [<Struct>]
    type SimpleElement = {
        rectangle: Rectangle
        color: Color
        content: TextContent option
    }

    type Element =
        | Button of btn:SimpleElement * TickFun
        | Text of text:SimpleElement
        | Border of color:BorderStyle * thickness:int * inner:Element
        | VerticalListBox of offset:Offset * e:Element list
        | HorizontalListBox of offset:Offset * e:Element list
        | CustomElement of Rectangle * cstmDraw:TickFun
        with
            member self.Rectangle =
                match self with
                | Button(data, _)
                | Text data -> data.rectangle
                | Border(_, thickness, inner) ->
                    let width = inner.Rectangle.Width + thickness * 2
                    let height = inner.Rectangle.Height + thickness * 2
                    rect inner.Rectangle.X inner.Rectangle.Y width height
                | VerticalListBox(rectangle, elements) ->
                    let width = elements |> List.maxBy(fun e -> e.Rectangle.Width)
                    let height = elements |> List.sumBy (fun e -> e.Rectangle.Height)
                    rect rectangle.X rectangle.Y width.Rectangle.Width height
                | HorizontalListBox(rectangle, elements) ->
                    let height = elements |> List.maxBy(fun e -> e.Rectangle.Height)
                    let width= elements |> List.sumBy (fun e -> e.Rectangle.Width)
                    rect rectangle.X rectangle.Y width height.Rectangle.Height
                | CustomElement(rect, _) -> rect
        
    [<Struct;RequireQualifiedAccess>]
    type LooseElement = {
        preferredSize:Vector2
        preferredOffset:Vector2 option
        color: Color
        content: TextContent option
    }

    [<Struct;RequireQualifiedAccess>]
    type ElementWithPosition = {
        rectangle: Rectangle
        color: Color
        content: TextContent option
    }

    [<RequireQualifiedAccess>]
    type UnprocessedElementTree =
        | Button of btn:LooseElement * TickFun
        | Text of text:LooseElement
        | Border of color:BorderStyle * thickness:int * inner:UnprocessedElementTree
        | VerticalListBox of maxSize:Vector2 * e:UnprocessedElementTree list
        | HorizontalListBox of maxSize:Vector2 * e:UnprocessedElementTree list
        | CustomElement of relativeOffsetAndAbsoluteSize:Rectangle * cstmDraw:TickFun
            
    [<RequireQualifiedAccess>]
    type ProcessedElementTree =
        | Button of btn:ElementWithPosition * TickFun
        | Text of text:ElementWithPosition
        | Border of color: ElementWithPosition * inner:ProcessedElementTree
        | VerticalListBox of position:Rectangle * e:ProcessedElementTree list
        | HorizontalListBox of position:Rectangle * e:ProcessedElementTree list
        | CustomElement of Rectangle * cstmDraw:TickFun
            
    
module ExternalCode =
    // This module uses code from the Xelmish project.

    let inline vector2 x y = Vector2(float32 x, float32 y)
    let inline isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

    /// Draw a coloured rect
    let colorRectRaw color (width, height) (x, y) =
        fun loadedAssets _ (spriteBatch:SpriteBatch) ->
            spriteBatch.Draw(loadedAssets.whiteTexture, rect x y width height, color)

    /// Run the given event if the left mouse button has just been pressed in the specified area
    let onclickWith event (width, height) (x, y) =
        OnDraw (fun loadedAssets inputs spriteBatch -> 
            if (inputs.mouseState.X, inputs.mouseState.Y) ||> isInside x y width height then
                if inputs.mouseState.LeftButton = ButtonState.Pressed 
                && inputs.lastMouseState.LeftButton <> ButtonState.Pressed then
                    event loadedAssets inputs spriteBatch)

    let text font (prefFontSize: float) colour (ox: float, oy: float) (text: string) (x, y) width =
        OnDraw (fun loadedAssets _ spriteBatch -> 
            let font = loadedAssets.fonts.[font]
            let measured = font.MeasureString (text)
            let scale = 
                //printfn "Text size of %s" text
                //printfn "%A" measured.X
                //printfn "%A" measured.Y
                //if measured.X > float32 width then
                //    let v = (float32 prefFontSize / measured.Y) * 0.5f in Vector2(v, v)
                //else
                    let v = float32 prefFontSize / measured.Y in Vector2(v, v)
            let origin = Vector2 (float32 ox * measured.X * scale.X, float32 oy * measured.Y * scale.Y)
            let position = Vector2.Add(origin, vector2 x y)
            spriteBatch.DrawString (font, text, position, colour, 0.f, Vector2.Zero, scale, SpriteEffects.None, 0.f))

open Model
open ExternalCode

//let layoutLooseElementTree (elementTree:UnprocessedElementTree) =
//    let rec proc currOffset =
//        match elementTree with
//        | Button(data, tickFun) ->
                
            
//    ()
    
let nest elements =
    OnDraw(fun loadedAssets input spriteBatch ->
        elements
        |> List.iter(function
            | OnDraw fn -> fn loadedAssets input spriteBatch
            | OnUpdate fn -> fn input
        )
    )

let mkTextPanel data font fontSize offset =
    let width = data.rectangle.Width
    let height = data.rectangle.Height
    let x = data.rectangle.X + offset.X
    let y = data.rectangle.Y + offset.Y
    [
        colour data.color (width, height) (x, y)
        // onclickWith event (width, height) (x, y)
    ] @
    [
        match data.content with
        | Some info ->
            yield
                text font fontSize info.color (-0.5, -0.5) info.text (x + width/2, y+height/2) width
        | None -> ()
    ] |> nest

let mkButton data event font fontSize offset =
    let width = data.rectangle.Width
    let height = data.rectangle.Height
    let x = data.rectangle.X + offset.X
    let y = data.rectangle.Y + offset.Y
    [
        colour data.color (width, height) (x, y)
        onclickWith event (width, height) (x, y)
    ] @
    [
        match data.content with
        | Some info ->
            yield
                text font fontSize info.color (-0.5, -0.5) info.text (x + width/2, y+height/2) width
        | None -> ()
    ] |> nest

let createViewableFromTree defaultFontName defaultFontSize tree =
    let rec drawElement elements offset =
        match elements with
        | Model.Button(data, fn) ->
            [ mkButton data fn defaultFontName defaultFontSize offset ]
        | Model.Text(data) ->
            [ mkTextPanel data defaultFontName defaultFontSize offset ]
        | Model.Border(color, thickness, inner) ->
                
            // Non in place border
            //let width = inner.Rectangle.Width + thickness * 2
            //let height = inner.Rectangle.Height + thickness * 2
            //let newOffset =
            //    let x = inner.Rectangle.X + thickness
            //    let y = inner.Rectangle.Y + thickness
            //    { X=x + offset.X; Y=y + offset.Y }
                
            // In-place border
            let width = inner.Rectangle.Width
            let height = inner.Rectangle.Height
            let newOffset =
                let x = inner.Rectangle.X + thickness
                let y = inner.Rectangle.Y + thickness
                { X=x + offset.X; Y=y + offset.Y }
            [   yield colour color (width, height) (inner.Rectangle.X + offset.X, inner.Rectangle.Y + offset.Y)
                yield! drawElement inner newOffset
            ]
        | Model.VerticalListBox(rect, elements) ->
            let newOffset = {
                X = offset.X + rect.X
                Y = offset.Y + rect.Y
            }
            elements
            |> List.fold(fun (acc, offset) element ->
                [   yield! acc
                    yield! drawElement element offset ], {
                    X = offset.X // + element.Rectangle.Width
                    Y = offset.Y + element.Rectangle.Height }
            ) ([], newOffset)
            |> fst
        | Model.HorizontalListBox(rect, elements) ->
            let newOffset = {
                X = offset.X + rect.X
                Y = offset.Y + rect.Y
            }
            elements
            |> List.fold(fun (acc, offset) element ->
                [   yield! acc
                    yield! drawElement element offset ], {
                    X = offset.X + element.Rectangle.Width
                    Y = offset.Y } //+ element.Rectangle.Height }
            ) ([], newOffset)
            |> fst
        | Model.CustomElement(_, cstmDraw) ->
            [ OnDraw(cstmDraw) ]
    drawElement tree { X=0;Y=0 }

/// Run the given event if the left mouse button has just been pressed in the specified area
let onhover (width, height) (x, y) event =
    OnDraw (fun loadedAssets inputs spriteBatch -> 
        if (inputs.mouseState.X, inputs.mouseState.Y) ||> ExternalCode.isInside x y width height then
                event loadedAssets inputs spriteBatch)

let drawRawElements elements loadedAssets input spriteBatch =
    elements |> List.iter(fun e -> e loadedAssets input spriteBatch)

let invertColor (color:Color) =
    Color(0xFFFFFFu - color.PackedValue)

let hoverEffect color (width, height) (x, y) getViewable =
    let borderThickness = 5
    let hoverEffect =
        let elements = [
            colorRectRaw color
                (width, height)
                (x, y)
            colorRectRaw (invertColor color)
                (width - borderThickness * 2, height - borderThickness * 2)
                (x + borderThickness, y + borderThickness)
        ]
        onhover (width, height) (x, y) (drawRawElements elements)
    [
        hoverEffect
        getViewable
            (width - borderThickness * 2, height - borderThickness * 2)
            (x + borderThickness, y + borderThickness)
    ]
    
    
let border color thickness inner = Border(color, thickness, inner)
let text data = Text data
let button data callback = Button(data, callback)
let verticalListBox offset elements = VerticalListBox(offset, elements)
let horizontalListBox offset elements = HorizontalListBox(offset, elements)

let simpleText x y color text = 
    {   color = Color.Transparent
        rectangle = rect x y 100 100
        content = Some {
            text = text
            color = color
        }
    } |> Text

let span x y width height color =
    {   color = color
        rectangle = rect x y width height
        content = None
    } |> Text