module FsBump.Desktop.Program

open Mibo.Elmish
open FsBump.Core
open FsBump.Core.Program

[<EntryPoint>]
let main _ =
  // Configure window here if needed, or pass config to program in Core
  let program =
    Program.create()
    |> Program.withConfig(fun (game, graphics) ->
      game.IsMouseVisible <- true
      game.Window.Title <- "FsBump Desktop"
      game.Window.AllowAltF4 <- true
      graphics.PreferredBackBufferWidth <- 1280
      graphics.PreferredBackBufferHeight <- 720)

  use game = new ElmishGame<Model, Msg>(program)
  game.Run()
  0
