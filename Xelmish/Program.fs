﻿[<RequireQualifiedAccess>]
module Xelmish.Program

open Elmish
open GameLoop
open Model

/// Entry point to run an Elmish application using Xelmish.
/// Expects a GameConfig variable with basic game loop setup config (e.g. resolution, assets)
let runGameLoop config (program: Program<_, _, _, ViewableOrInteraction list>) =
    let rec setState model dispatch =
        loop.View <- program.view model dispatch
    and startProgramFn arg =
        Program.runWith arg { program with setState = setState }
    and loop : GameLoop = new GameLoop (config, startProgramFn)
    loop.Run ()

/// Alternative entry point to Xelmish from Elmish.
/// Accepts variables to configure common config properties for the game loop
let runSimpleGameLoop assetsToLoad (windowWidth, windowHeight) clearColour (program: Program<_, _, _, ViewableOrInteraction list>) =
    let config = {
        resolution = Windowed (windowWidth, windowHeight)
        clearColour = Some clearColour
        mouseVisible = true
        assetsToLoad = assetsToLoad }
    runGameLoop config program
