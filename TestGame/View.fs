module View

open Model
open Update

open System

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

let draw content gameState dispatch : unit =
    let spriteBatch = content.spriteBatch
    spriteBatch.GraphicsDevice.Clear(Color.Gray)
    let borderThickness = 5.f
    let scalingFactor =
        let widthScalingFactor = (float32 spriteBatch.GraphicsDevice.Viewport.Width - borderThickness * 2.f) / float32 Update.worldWidth
        let heightScalingFactor = (float32 spriteBatch.GraphicsDevice.Viewport.Height - borderThickness * 2.f) / float32 Update.worldHeight
        min widthScalingFactor heightScalingFactor
        
    let scaledWorldWidth, scaledWorldHeight =
        float32 Update.worldWidth * scalingFactor,
        float32 Update.worldHeight * scalingFactor //- borderThickness * 2
    let color = Color.Red
    let drawSquare color pos = 
        let x = pos.X |> float32
        let y = pos.Y |> float32
        let transformedRect =
            Rectangle(
                x * scalingFactor + borderThickness |> int, float32 pos.Y * scalingFactor + borderThickness |> int,
                scalingFactor |> int, scalingFactor |> int)
        spriteBatch.Draw(content.rect, transformedRect, color)
    spriteBatch.Begin()
    match gameState with
    | GameIsRunning model ->
        // Draw world bounds
        do
            let borderColor = Color.Black
            let borderWidth = scaledWorldWidth + borderThickness * 2.f |> int
            let borderHeight = scaledWorldHeight + borderThickness * 2.f |> int
            // Upper border
            spriteBatch.Draw(content.rect, Rectangle(0, 0, borderWidth, borderThickness |> int), borderColor)
            // Right border
            spriteBatch.Draw(content.rect, Rectangle(borderWidth - int borderThickness, 0, int borderThickness, borderHeight), borderColor)
            // Left border
            spriteBatch.Draw(content.rect, Rectangle(0, 0, int borderThickness, borderHeight), borderColor)
            // Bottom border
            spriteBatch.Draw(content.rect, Rectangle(0, borderHeight - int borderThickness, borderWidth, int borderThickness), borderColor)
        for x in 0..(Update.worldWidth - 1) do
            for y in 0..(Update.worldWidth - 1) do
                let currentColor =
                    if x % 2 = 0 && y % 2 = 0 ||
                        x % 2 <> 0 && y % 2 <> 0 then
                        Color.LightGray
                        (*
                        +012
                        0xyx
                        1yxy
                        2xyx
                        *)
                    else
                        Color.DarkGray
                drawSquare currentColor { X=x; Y=y }
        match model.snakeParts with
        | head :: tail ->
            drawSquare Color.Black head
            tail |> List.iteri (fun i e -> 
                let currColor = if i % 2 = 0 then Color.Red else Color.Orange
                drawSquare currColor e)
        | [] ->
            raise (InvalidOperationException "Invalid state detected, the snake always has a head")
        drawSquare Color.Orange model.food
    | GameOver ->
        spriteBatch.DrawString(content.font, "Game over", Vector2(50.f, 50.f), color)
    
    spriteBatch.End()
        

