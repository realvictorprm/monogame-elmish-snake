open Game
open System.Threading
// This file is uninteresting, look into Game.fs which contains all the logic :)!
// The comments should explain everything necessary, for basic F# questions or
// if you're generally interested in F# learn more about F# at http://fsharp.org
// or highly recommended browse https://fsharpforfunandprofit.com !

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    printfn "Hello World!"
    (
        use game = new MainGame()
        game.Run()
    )
    
    0 // return an integer exit code
