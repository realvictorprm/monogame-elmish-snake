// Module containing the implementation of the "game".
module Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio
open System
open Microsoft.Xna.Framework.Content
open Model
open System.Collections.Concurrent
open System.Collections.Generic

// The content model holding all textures and sounds used in the game
// and the object for drawing in the Draw function later.
// Side note:
// Comparison is not possible as all data fields are not compareable per default.

// The empty model:
// To the beginning the logo position and size is both zero
// as the runtime values aren't known yet.
let emptyModel : Model = GameOver

type MainGame() as self =
    inherit Game()

    let graphicsDeviceManager = new GraphicsDeviceManager(self)

    let mutable dispatchOption : (Msg -> unit) option = None

    let mutable currentDrawTask = fun () -> ()

    let mutable lastStateChangeHasBeenDrawn = false

    do
        // Constructor init calls
        self.Content.RootDirectory <- "Content"
        self.Window.AllowUserResizing <- true
        self.IsMouseVisible <- true

    override self.Initialize() =
        base.Initialize()

    override self.LoadContent() =
        // For clean seperation of the logic a method for loading the content is used.
        // Moreover prior to this point the content model isn't filled yet.
        let content = Update.loadContent(graphicsDeviceManager) self.Content
        async {
            let initDispatcher _ =
                let sub dispatch =
                    dispatchOption <- dispatch |> Some
                    Update.subscriptions
                    |> List.iter (fun fn -> fn dispatch)
                Elmish.Cmd.ofSub sub
            let temp = View.draw content
            // The drawing is delayed and won't be applied instantly.
            let view model dispatch = currentDrawTask <- (fun () -> temp model dispatch)
            Elmish.Program.mkProgram Update.init (Update.update self) view
            |> Elmish.Program.withSubscription initDispatcher
            |> Elmish.Program.run
        } |> Async.Start
        
    override __.UnloadContent() =
        // Nothing to do here yet.
        ()

    // This update method is calling the model update method if the model has a valid value.
    override self.Update(gameTime) =
        match dispatchOption with
        | Some dispatch ->
            let keyState = Input.Keyboard.GetState()
            let mouseState = Input.Mouse.GetState()

            // Calculate the new game state
            UpdateTick(gameTime, keyState, mouseState)
            |> dispatch

            lastStateChangeHasBeenDrawn <- false
        | None -> ()
        base.Update(gameTime)
         
    // This draw method is calling the actual draw method if both the data model
    // and content model have a valid value.
    override self.Draw(gameTime) =
        // Check if we need to refresh the state first before we redraw
        if lastStateChangeHasBeenDrawn then
            match dispatchOption with
            | Some dispatch -> RedrawRequested |> dispatch
            | None -> ()
            self.Draw(gameTime)
        currentDrawTask ()
        lastStateChangeHasBeenDrawn <- false