module Utils
open Microsoft.Xna.Framework

open Model

module Rectangle =
    let WithLocation location (rectangle:Rectangle) =
        Rectangle(location, rectangle.Size)
    let WithSize size (rectangle:Rectangle) =
        Rectangle(rectangle.Location, size)

module Math =
    // Calculation inspired from https://sebadorn.de/2012/01/02/herausfinden-ob-ein-punkt-in-einer-ellipse-liegt
    // No rotation is respected.
    let isPointInEllipse (point:Vector2) (rectangle:Rectangle) =
        let px = point.X
        let py = point.Y
        let rx = float32 rectangle.Width / 2.0f
        let ry = float32 rectangle.Height / 2.0f
        let cx = float32 rectangle.X
        let cy = float32 rectangle.Y
        ((px - cx) ** 2.f / rx ** 2.f) + ((py - cy) ** 2.f / ry ** 2.f) <= 1.0f

module Vector2 =
    let inline add a b = Vector2.Add(a, b)
    let inline multiply (a:Vector2) (b:Vector2) = Vector2.Multiply(a, b)
    let inline divide (a:Vector2) (b:Vector2) = Vector2.Divide(a, b)

[<RequireQualifiedAccess>]
type Vector2Operators =
     static member inline (*) (a:Vector2, b:Vector2) = Vector2.Multiply(a, b)
     static member inline (+) (a:Vector2, b:Vector2) = Vector2.Add(a, b)
     static member inline (-) (a:Vector2, b:Vector2) = Vector2.Subtract(a, b)
     static member inline (/) (a:Vector2, b:Vector2) = Vector2.Divide(a, b)
     static member inline (/) (a:Vector2, b:float32) = Vector2.Divide(a, b)

[<RequireQualifiedAccess>]
type MatrixOperators =
     static member inline (*) (a:Matrix, b:Matrix) = Matrix.Multiply(a, b)

let inline vec2 a b = Vector2(a, b)


let getTranslationFromDirection direction =
    match direction with
    | Up -> 0, -1
    | Down -> 0, 1
    | Left -> -1, 0
    | Right -> 1, 0

let getInvertedDirection direction =
    match direction with
    | Up -> Down
    | Left -> Right
    | Down -> Up
    | Right -> Left

type Camera with
    member self.Translate vec =
        { self with position = Vector2.Add(vec, self.position) }

    static member Transformation camera width height =
        Matrix.CreateTranslation(Vector3(-camera.position, 0.f)) *
        Matrix.CreateScale(camera.zoom) *
        Matrix.CreateTranslation(Vector3(width / 2 |> float32, height / 2 |> float32, 0.f))

type Zoom with
    static member toValue zoomState =
        match zoomState with
        | Near -> 8
        | Middle -> 3
        | Far -> 1
