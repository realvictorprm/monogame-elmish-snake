open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Update
open View
open Microsoft.Xna.Framework

// The comments should explain everything necessary, for basic F# questions or
// if you're generally interested in F# learn more about F# at http://fsharp.org
// or highly recommended browse https://fsharpforfunandprofit.com !

[<EntryPoint>]
let main argv = 
    let width, height = 1280, 800
    let config = {
        resolution = Windowed (width, height)
        clearColour = Some Colour.White // if set to None, then each draw will layer over the previous. which looks weird.
        mouseVisible = true
        assetsToLoad = [
            PipelineFont ("font", "./content/font") // the font used in the game needs to be loaded. there is no built-in font.
            PipelineSound ("bang", "./content/Logo_hit")
        ]
        stretchMode = StretchMode.Blended
    }

    let adaptedInit = Microsoft.Xna.Framework.Point(width, height) |> init
    let adaptedView model dispatch = viewAndInteractions model dispatch

    Program.mkProgram adaptedInit update viewAndInteractions // standard, out of the box Elmish initialisation
    // |> Program.withConsoleTrace // standard, out of the box Elmish console tracing.
    |> Program.withSubscription(fun _ -> subscriptions)
    |> Xelmish.Program.runGameLoop config // Xelmish specific run function
    0
