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