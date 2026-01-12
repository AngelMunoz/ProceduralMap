namespace ProceduralMap

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish.Graphics3D

module Skybox =

  type State = { Time: float32 }

  let init() = { Time = 0.0f }

  let update dt state = { state with Time = state.Time + dt }

  let draw
    (modelStore: IModelStore)
    (cameraPosition: Vector3)
    (effect: Effect)
    (state: State)
    (buffer: RenderBuffer<RenderCmd3D>)
    =
    modelStore.Get "cube"
    |> Option.iter(fun mesh ->
      // Huge scale to ensure it's outside the playable area
      // Centered on camera so it stays "at infinity" relative to the player
      let world =
        Matrix.CreateScale(1000.0f)
        * Matrix.CreateTranslation(cameraPosition)

      Draw3D.mesh mesh world
      (*
      |> Draw3D.withEffect(fun fx ctx ->
        // Skybox view matrix: strip translation so it's always centered
        let view = ctx.View

        let staticView =
          Matrix.CreateWorld(Vector3.Zero, view.Forward, view.Up)

        fx.Parameters.["World"].SetValue(world)
        fx.Parameters.["View"].SetValue(staticView)
        fx.Parameters.["Projection"].SetValue(ctx.Projection)
        fx.Parameters.["Time"].SetValue(state.Time))
      *)
      |> Draw3D.submit buffer)
