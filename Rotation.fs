namespace ProceduralMap

open Microsoft.Xna.Framework

module Rotation =

  module private Internal =
    /// Compute rotation delta from velocity (rolling ball effect)
    let computeDelta (dt: float32) (velocity: Vector3) =
      let rollX = velocity.Z * dt * Physics.Constants.RollSpeed
      let rollZ = -velocity.X * dt * Physics.Constants.RollSpeed
      Quaternion.CreateFromYawPitchRoll(0.0f, rollX, rollZ)

  /// Calculate new rotation based on current velocity and state
  let update (dt: float32) (velocity: Vector3) (current: Quaternion) =
    Internal.computeDelta dt velocity
    |> (fun d -> Quaternion.Concatenate(current, d))
