namespace ProceduralMap

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input.Touch
open Mibo.Input

module TouchLogic =

  type TouchZone =
    | LeftSide // Joystick
    | RightSide // Jump

  type JoystickState = {
    Center: Vector2
    Current: Vector2
    ActiveId: int option
  }

  type State = {
    Joystick: JoystickState
    JumpTriggered: bool
    ScreenSize: Vector2
  }

  let init (screenSize: Vector2) = {
    Joystick = {
      Center = Vector2.Zero
      Current = Vector2.Zero
      ActiveId = None
    }
    JumpTriggered = false
    ScreenSize = screenSize
  }

  let private getZone (pos: Vector2) (screenSize: Vector2) =
    if pos.X < screenSize.X * 0.5f then LeftSide else RightSide

  let update (screenSize: Vector2) (state: State) =
    let touches = TouchPanel.GetState()
    let mutable nextJoystick = state.Joystick
    let mutable jumpTriggered = false

    // Reset joystick if touch ended
    match state.Joystick.ActiveId with
    | Some id ->
      let found =
        touches
        |> Seq.tryFind (fun t -> t.Id = id && t.State <> TouchLocationState.Released)

      match found with
      | Some t -> nextJoystick <- { nextJoystick with Current = t.Position }
      | None ->
        nextJoystick <- {
          Center = Vector2.Zero
          Current = Vector2.Zero
          ActiveId = None
        }
    | None -> ()

    // Process new touches
    for touch in touches do
      match touch.State with
      | TouchLocationState.Pressed ->
        match getZone touch.Position screenSize with
        | LeftSide when nextJoystick.ActiveId.IsNone ->
          nextJoystick <- {
            Center = touch.Position
            Current = touch.Position
            ActiveId = Some touch.Id
          }
        | RightSide -> jumpTriggered <- true
        | _ -> ()
      | _ -> ()

    {
      state with
          Joystick = nextJoystick
          JumpTriggered = jumpTriggered
          ScreenSize = screenSize
    }

  /// Map touch state to PlayerAction state
  let toActionState (state: State) =
    let mutable held = Set.empty
    let mutable started = Set.empty

    // 1. Joystick Movement
    match state.Joystick.ActiveId with
    | Some _ ->
      let diff = state.Joystick.Current - state.Joystick.Center
      let deadzone = 10.0f
      let maxDist = 100.0f

      if diff.Length() > deadzone then
        if diff.Y < -deadzone then held <- held.Add MoveForward
        if diff.Y > deadzone then held <- held.Add MoveBackward
        if diff.X < -deadzone then held <- held.Add MoveLeft
        if diff.X > deadzone then held <- held.Add MoveRight
    | None -> ()

    // 2. Jump
    if state.JumpTriggered then
      started <- started.Add Jump

    let baseInput: ActionState<PlayerAction> = ActionState.empty
    { baseInput with Held = held; Started = started }

open Mibo.Elmish.Graphics3D

module TouchUI =
  open Mibo.Elmish.Graphics2D

  let draw (state: TouchLogic.State) (buffer: RenderBuffer<RenderCmd2D>) =
    match state.Joystick.ActiveId with
    | Some _ ->
      let center = state.Joystick.Center
      let current = state.Joystick.Current
      
      // We don't have the texture yet, but we'll try to call Draw2D.sprite
      // to see if it exists and what its signature might be.
      // Usually: texture, position, color
      // Draw2D.sprite null center Color.White |> Draw2D.submit buffer
      ()
    | None -> ()
