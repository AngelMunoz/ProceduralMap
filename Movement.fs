namespace ProceduralMap

open Microsoft.Xna.Framework
open Mibo.Input

module Movement =

  module private Internal =
    /// Compute movement direction from action state without mutability
    let computeDirection(input: ActionState<PlayerAction>) =
      let x =
        (if input.Held.Contains MoveRight then 1.0f else 0.0f)
        - (if input.Held.Contains MoveLeft then 1.0f else 0.0f)

      let z =
        (if input.Held.Contains MoveBackward then 1.0f else 0.0f)
        - (if input.Held.Contains MoveForward then 1.0f else 0.0f)

      let dir = Vector3(x, 0.0f, z)

      if dir.LengthSquared() > 0.0f then
        Vector3.Normalize(dir)
      else
        dir

    /// Apply acceleration towards target or friction
    let applyPhysics (dt: float32) (moveDir: Vector3) (currentVel: Vector3) =
      let horizontal = Vector2(currentVel.X, currentVel.Z)
      let hasInput = moveDir.LengthSquared() > 0.0f

      let nextHorizontal =
        if hasInput then
          let target =
            Vector2(moveDir.X, moveDir.Z) * Physics.Constants.MoveSpeed

          let diff = target - horizontal
          let step = Physics.Constants.Acceleration * dt

          if diff.Length() <= step then
            target
          else
            horizontal + Vector2.Normalize(diff) * step
        else
          let decel = Physics.Constants.Friction * dt
          let speed = horizontal.Length()

          if speed <= decel then
            Vector2.Zero
          else
            horizontal * ((speed - decel) / speed)

      Vector3(nextHorizontal.X, currentVel.Y, nextHorizontal.Y)

  /// Update player movement velocity based on input
  let update (dt: float32) (input: ActionState<PlayerAction>) (body: Body) =
    let vel' =
      Internal.computeDirection input
      |> (fun d -> Internal.applyPhysics dt d body.Velocity)

    { body with Velocity = vel' }
