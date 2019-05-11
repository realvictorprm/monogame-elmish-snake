open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Update
open View

// The comments should explain everything necessary, for basic F# questions or
// if you're generally interested in F# learn more about F# at http://fsharp.org
// or highly recommended browse https://fsharpforfunandprofit.com !

[<EntryPoint>]
let main argv = 
    let config = {
        resolution = Windowed (800, 600)
        clearColour = Some Colour.White // if set to None, then each draw will layer over the previous. which looks weird.
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("font", "./content/font") // the font used in the game needs to be loaded. there is no built-in font.
        ]
        stretchMode = StretchMode.PointClamp
    }

    Program.mkProgram init update view // standard, out of the box Elmish initialisation
    |> Program.withConsoleTrace // standard, out of the box Elmish console tracing.
    |> Program.withSubscription(fun _ -> subscriptions)
    |> Xelmish.Program.runGameLoop config // Xelmish specific run function
    0
