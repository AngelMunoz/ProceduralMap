namespace ProceduralMap

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Input

type CollisionType =
  | Solid
  | Passthrough
  | Climbable
  | Slope

type TileType =
  | Floor
  | Wall
  | Platform
  | Spikes
  | Collectible
  | StartPoint
  | Decoration
  | SlopeTile

type Tile = {
  Type: TileType
  Collision: CollisionType
  Position: Vector3
  Rotation: float32
  Variant: int
  Size: Vector3
  Style: int
  AssetName: string
  VisualOffset: Vector3
}

type PathState = {
  Position: Vector3
  Direction: Vector3
  PreviousDirection: Vector3
  Width: int
  CurrentColor: int
}

type GenerationConfig = {
  MaxJumpHeight: float32
  MaxJumpDistance: float32
  SafetyBuffer: float32
}

type Body = {
  Position: Vector3
  Velocity: Vector3
  Radius: float32
}

type PlayerAction =
  | MoveForward
  | MoveBackward
  | MoveLeft
  | MoveRight
  | Jump

type PlayerModel = {
  Body: Body
  Input: ActionState<PlayerAction>
  IsGrounded: bool
  Rotation: Quaternion
  LastSafePosition: Vector3
}

type ModelGeometry = { Vertices: Vector3[]; Indices: int[] }

type IModelStore =
  abstract member Load: string -> unit
  abstract member Get: string -> Model option
  abstract member GetBounds: string -> BoundingBox option
  abstract member GetGeometry: string -> ModelGeometry option
