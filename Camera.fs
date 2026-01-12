namespace ProceduralMap

open Microsoft.Xna.Framework

module Camera =

  type State = { Position: Vector3; Target: Vector3 }

  /// Smoothly update camera position and target to follow the player
  let update (dt: float32) (playerPosition: Vector3) (state: State) : State =
    let targetPos = playerPosition + Vector3(0.0f, 15.0f, 12.0f)

    {
      Position = Vector3.Lerp(state.Position, targetPos, dt * 2.5f)
      Target = Vector3.Lerp(state.Target, playerPosition, dt * 5.0f)
    }
