namespace ProceduralMap

open Mibo.Elmish
open Mibo.Input
open Microsoft.Xna.Framework

module Player =

  module Input =
    open Microsoft.Xna.Framework.Input

    let config =
      InputMap.empty
      |> InputMap.key MoveForward Keys.W
      |> InputMap.key MoveForward Keys.Up
      |> InputMap.key MoveBackward Keys.S
      |> InputMap.key MoveBackward Keys.Down
      |> InputMap.key MoveLeft Keys.A
      |> InputMap.key MoveLeft Keys.Left
      |> InputMap.key MoveRight Keys.D
      |> InputMap.key MoveRight Keys.Right
      |> InputMap.key Jump Keys.Space

  module Operations =
    /// Initialize player state
    let create initialPos = {
      Body = {
        Position = initialPos
        Velocity = Vector3.Zero
        Radius = 0.5f
      }
      Input = ActionState.empty
      IsGrounded = false
      Rotation = Quaternion.Identity
      LastSafePosition = initialPos
    }

    /// Orchestrate one tick of player logic
    let updateTick
      (dt: float32)
      (modelStore: IModelStore)
      (map: Tile list)
      (model: PlayerModel)
      =
      let jumpRequested = model.Input.Started.Contains Jump

      // 1. Movement System
      let bodyAfterMovement: Body = Movement.update dt model.Input model.Body

      // 2. Physics System
      let nearbyTiles =
        map
        |> List.filter(fun (t: Tile) ->
          Vector3.DistanceSquared(t.Position, bodyAfterMovement.Position) < 1600.0f)

      let struct (bodyAfterPhysics, isGrounded) =
        Physics.updateBody
          dt
          bodyAfterMovement
          nearbyTiles
          modelStore
          jumpRequested
          model.IsGrounded

      // 3. Rotation System
      let newRotation =
        Rotation.update dt bodyAfterPhysics.Velocity model.Rotation

      // 4. Safety & Respawn logic
      let killFloor =
        if map.IsEmpty then
          -20.0f
        else
          (map |> List.minBy(fun t -> t.Position.Y)).Position.Y - 15.0f

      let nextLastSafePos =
        if isGrounded then
          nearbyTiles
          |> List.tryFind(fun t ->
            Vector3.DistanceSquared(t.Position, bodyAfterPhysics.Position) < 16.0f)
          |> Option.map(fun t -> t.Position + Vector3.Up * 1.0f)
          |> Option.defaultValue model.LastSafePosition
        else
          model.LastSafePosition

      if bodyAfterPhysics.Position.Y < killFloor then
        {
          model with
              Body = {
                bodyAfterPhysics with
                    Position = model.LastSafePosition + Vector3.Up * 2.5f
                    Velocity = Vector3.Zero
              }
              IsGrounded = false
              Rotation = Quaternion.Identity
              Input = {
                model.Input with
                    Held = Set.empty
                    Started = Set.empty
              }
        }
      else
        {
          model with
              Body = bodyAfterPhysics
              IsGrounded = isGrounded
              Rotation = newRotation
              LastSafePosition = nextLastSafePos
        }

  type Msg = InputChanged of ActionState<PlayerAction>

  let init initialPos = Operations.create initialPos, Cmd.none

  let update msg (model: PlayerModel) =
    match msg with
    | InputChanged input -> { model with Input = input }, Cmd.none
