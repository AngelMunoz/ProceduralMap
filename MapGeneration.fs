namespace ProceduralMap

open System
open Microsoft.Xna.Framework

// ─────────────────────────────────────────────────────────────
// Tile Construction
// ─────────────────────────────────────────────────────────────

module TileBuilder =

  let create type' collision pos rot variant size style asset offset = {
    Type = type'
    Collision = collision
    Position = pos
    Rotation = rot
    Variant = variant
    Size = size
    Style = style
    AssetName = asset
    VisualOffset = offset
  }

  let getAssetData asset color (modelStore: IModelStore) =
    let colorStr =
      match color % 4 with
      | 0 -> "blue"
      | 1 -> "green"
      | 2 -> "red"
      | _ -> "yellow"

    let full = sprintf "kaykit_platformer/%s/%s_%s" colorStr asset colorStr

    match modelStore.GetBounds full with
    | Some b -> b.Max - b.Min, -((b.Min + b.Max) * 0.5f), asset
    | None -> Vector3.One, -Vector3.Up * 0.5f, asset

  let floor pos color modelStore =
    let size, offset, asset = getAssetData "platform_1x1x1" color modelStore
    create TileType.Floor CollisionType.Solid pos 0.0f color size 0 asset offset

  let wall pos color size asset offset =
    create TileType.Wall CollisionType.Solid pos 0.0f color size 0 asset offset

  let decoration pos rot color size asset offset type' collision =
    create type' collision pos rot color size 0 asset offset

  let platform pos color size assetName offset =
    create
      TileType.Platform
      CollisionType.Solid
      pos
      0.0f
      color
      size
      0
      assetName
      offset

  let collectible pos color size asset offset =
    create
      TileType.Collectible
      CollisionType.Passthrough
      pos
      0.0f
      color
      size
      0
      asset
      offset

  let slope pos rot color size asset offset =
    create
      TileType.SlopeTile
      CollisionType.Slope
      pos
      rot
      color
      size
      0
      asset
      offset

// ─────────────────────────────────────────────────────────────
// Validation
// ─────────────────────────────────────────────────────────────

module Validation =
  let getTileBounds (t: Tile) (buffer: float32) =
    let half = t.Size * 0.5f + Vector3(buffer, buffer, buffer)
    BoundingBox(t.Position - half, t.Position + half)

  let checkOverlap
    (newTiles: Tile list)
    (obstacles: Tile list)
    (config: GenerationConfig)
    =
    let obstacleBoxes =
      obstacles |> List.map(fun t -> getTileBounds t config.SafetyBuffer)

    let checkBoxes =
      newTiles
      |> List.map(fun t -> getTileBounds t -(config.SafetyBuffer + 0.05f))

    checkBoxes
    |> List.exists(fun cb ->
      obstacleBoxes |> List.exists(fun ob -> cb.Intersects(ob)))

// ─────────────────────────────────────────────────────────────
// State Operations
// ─────────────────────────────────────────────────────────────

module StateOps =
  let advance dist (state: PathState) = {
    state with
        Position = state.Position + state.Direction * dist
  }

  let moveY offset (state: PathState) = {
    state with
        Position = state.Position + Vector3.Up * offset
  }

  let turn angle (state: PathState) =
    let nextDir =
      Vector3.Transform(state.Direction, Matrix.CreateRotationY(angle))

    let roundedDir =
      Vector3(
        MathF.Round(nextDir.X),
        MathF.Round(nextDir.Y),
        MathF.Round(nextDir.Z)
      )

    {
      state with
          Direction = roundedDir
          PreviousDirection = state.Direction
    }

// ─────────────────────────────────────────────────────────────
// Building Blocks
// ─────────────────────────────────────────────────────────────

module BuildingBlocks =

  let addEdgeDetails
    (rng: Random)
    (pos: Vector3)
    (color: int)
    (isLeft: bool)
    (modelStore: IModelStore)
    (tiles: ResizeArray<Tile>)
    =
    let roll = rng.NextDouble()

    if roll < 0.15 then
      let asset =
        match rng.Next(0, 3) with
        | 0 -> "barrier_1x1x1"
        | 1 -> "barrier_1x1x2"
        | _ -> "barrier_2x1x1"

      let size, offset, assetName =
        TileBuilder.getAssetData asset color modelStore

      tiles.Add(
        TileBuilder.wall
          (pos + Vector3.Up * size.Y * 0.5f + Vector3.Up * 0.5f)
          color
          size
          assetName
          offset
      )
    elif roll < 0.3 then
      let asset =
        match rng.Next(0, 3) with
        | 0 -> "flag_A"
        | 1 -> "signage_arrow_stand"
        | _ -> "flag_B"

      let size, offset, assetName =
        TileBuilder.getAssetData asset color modelStore

      let rot = if isLeft then -MathHelper.PiOver2 else MathHelper.PiOver2

      tiles.Add(
        TileBuilder.decoration
          (pos + Vector3.Up * size.Y * 0.5f + Vector3.Up * 0.5f)
          rot
          color
          size
          assetName
          offset
          TileType.Decoration
          CollisionType.Passthrough
      )
    elif roll < 0.4 then
      let asset = if rng.Next(0, 2) = 0 then "cone" else "button_base"

      let size, offset, assetName =
        TileBuilder.getAssetData asset color modelStore

      let rot = float32(rng.NextDouble()) * MathHelper.TwoPi

      tiles.Add(
        TileBuilder.decoration
          (pos + Vector3.Up * size.Y * 0.5f + Vector3.Up * 0.5f)
          rot
          color
          size
          assetName
          offset
          TileType.Decoration
          CollisionType.Climbable
      )

  let addRow
    (rng: Random)
    (modelStore: IModelStore)
    (tiles: ResizeArray<Tile>)
    (state: PathState)
    =
    let nextState = state |> StateOps.advance 1.0f
    let right = Vector3.Cross(Vector3.Up, state.Direction)

    for w in -state.Width .. state.Width do
      let tilePos = nextState.Position + right * float32 w
      tiles.Add(TileBuilder.floor tilePos state.CurrentColor modelStore)

      if abs w = state.Width then
        addEdgeDetails rng tilePos state.CurrentColor (w < 0) modelStore tiles

    nextState

  let addSlope
    (rng: Random)
    (modelStore: IModelStore)
    (tiles: ResizeArray<Tile>)
    (prevHalf: float32)
    (state: PathState)
    =
    let goUp = rng.NextDouble() > 0.5

    let size, offset, assetBase =
      TileBuilder.getAssetData
        "platform_slope_4x2x2"
        state.CurrentColor
        modelStore

    let currentHalf = size.Z * 0.5f

    let gap = if rng.NextDouble() < 0.2 then 2.0f else -0.25f
    let dist = prevHalf + currentHalf + gap

    let yOffset = if goUp then 0.0f else -size.Y
    let nextYOffset = if goUp then size.Y else -size.Y

    let rot =
      if state.Direction = -Vector3.UnitZ then
        0.0f
      elif state.Direction = Vector3.UnitZ then
        MathHelper.Pi
      elif state.Direction = -Vector3.UnitX then
        MathHelper.PiOver2
      else
        -MathHelper.PiOver2

    let finalRot = if goUp then rot else rot + MathHelper.Pi

    let slopeCenter =
      state.Position
      + state.Direction * dist
      + Vector3.Up * (size.Y * 0.5f + 0.5f + yOffset)

    tiles.Add(
      TileBuilder.slope
        slopeCenter
        finalRot
        state.CurrentColor
        size
        assetBase
        offset
    )

    state |> StateOps.advance dist |> StateOps.moveY nextYOffset, currentHalf

  let addPlatform
    (rng: Random)
    (modelStore: IModelStore)
    (tiles: ResizeArray<Tile>)
    (state: PathState)
    =
    let gapSize =
      if rng.NextDouble() < 0.8 then
        rng.Next(1, 3)
      else
        rng.Next(3, 5)

    let assets = [
      "platform_1x1x1"
      "platform_2x2x1"
      "platform_4x4x1"
      "platform_6x6x1"
    ]

    let size, offset, assetBase =
      TileBuilder.getAssetData
        assets.[rng.Next(assets.Length)]
        state.CurrentColor
        modelStore

    let heightChange = float32(rng.Next(-1, 2)) * 2.0f
    let dist = 0.5f + float32 gapSize + size.Z * 0.5f

    let nextPos =
      state.Position + state.Direction * dist + Vector3.Up * heightChange

    tiles.Add(
      TileBuilder.platform nextPos state.CurrentColor size assetBase offset
    )

    { state with Position = nextPos } |> StateOps.advance(size.Z * 0.5f - 0.5f)

// ─────────────────────────────────────────────────────────────
// Segment Strategies
// ─────────────────────────────────────────────────────────────

module SegmentStrategies =

  let flatRun
    (rng: Random)
    (length: int)
    (modelStore: IModelStore)
    (state: PathState)
    =
    let tiles = ResizeArray<Tile>()
    let mutable s = state

    if s.Direction <> s.PreviousDirection then
      for _ in 1 .. s.Width * 2 do
        s <- s |> BuildingBlocks.addRow rng modelStore tiles

    for _ in 1..length do
      s <- s |> BuildingBlocks.addRow rng modelStore tiles

    List.ofSeq tiles, s

  let slope
    (rng: Random)
    (length: int)
    (modelStore: IModelStore)
    (state: PathState)
    =
    let tiles = ResizeArray<Tile>()
    let mutable s = state
    let mutable ph = 0.5f

    for _ in 1 .. Math.Max(1, length / 2) do
      let nextS, nextPh = s |> BuildingBlocks.addSlope rng modelStore tiles ph
      s <- nextS
      ph <- nextPh

    List.ofSeq tiles, s

  let platform
    (rng: Random)
    (count: int)
    (modelStore: IModelStore)
    (state: PathState)
    =
    let tiles = ResizeArray<Tile>()
    let mutable s = state

    for _ in 1..count do
      s <- s |> BuildingBlocks.addPlatform rng modelStore tiles

    List.ofSeq tiles, s

// ─────────────────────────────────────────────────────────────
// Map Generator
// ─────────────────────────────────────────────────────────────

module MapGenerator =

  let createInitialState() = {
    Position = Vector3(0.0f, 0.0f, 0.0f)
    Direction = -Vector3.UnitZ
    PreviousDirection = -Vector3.UnitZ
    Width = 2
    CurrentColor = 0
  }

  let getSpawnPoint() = Vector3(0.0f, 4.0f, 0.0f)

  let generateSegment
    (rng: Random)
    (state: PathState)
    (obstacles: Tile list)
    (config: GenerationConfig)
    (modelStore: IModelStore)
    =
    let relevantObstacles =
      obstacles
      |> List.filter(fun t ->
        let box = Validation.getTileBounds t 0.5f
        box.Contains(state.Position) = ContainmentType.Disjoint)

    let rec retry attempt =
      let segmentType = rng.Next(0, 4)
      let length = rng.Next(10, 20)

      let tiles, endState =
        match segmentType with
        | 1 -> state |> SegmentStrategies.slope rng length modelStore
        | 2 -> state |> SegmentStrategies.platform rng (length / 2) modelStore
        | _ -> state |> SegmentStrategies.flatRun rng length modelStore

      let nextState =
        {
          endState with
              CurrentColor = (state.CurrentColor + 1) % 4
        }
        |> StateOps.turn(
          match rng.Next(0, 3) with
          | 1 -> MathHelper.PiOver2
          | 2 -> -MathHelper.PiOver2
          | _ -> 0.0f
        )

      if not(Validation.checkOverlap tiles relevantObstacles config) then
        tiles, nextState
      elif attempt < 5 then
        retry(attempt + 1)
      else
        let t, s = state |> SegmentStrategies.flatRun rng 5 modelStore

        t,
        {
          s with
              PreviousDirection = state.Direction
        }

    retry 0

  module Operations =
    let needsUpdate (playerPosition: Vector3) (pathState: PathState) =
      Vector3.Distance(playerPosition, pathState.Position) < 120.0f

    /// Handle infinite map generation and cleanup
    let updateMap
      (rng: Random)
      (modelStore: IModelStore)
      (playerPosition: Vector3)
      (map: Tile list)
      (pathState: PathState)
      =
      let map', state', generated =
        if needsUpdate playerPosition pathState then
          let config = {
            MaxJumpHeight = 2.8f
            MaxJumpDistance = 7.0f
            SafetyBuffer = 0.1f
          }

          let obstacles =
            map
            |> List.filter(fun t ->
              Vector3.DistanceSquared(t.Position, pathState.Position) < 2500.0f)

          let newTiles, nextState =
            generateSegment rng pathState obstacles config modelStore

          map @ newTiles, nextState, true
        else
          map, pathState, false

      // Optimization: Cleanup distant tiles
      let finalMap =
        map'
        |> List.filter(fun t ->
          Vector3.DistanceSquared(t.Position, playerPosition) < 40000.0f)

      if generated || finalMap.Length <> map.Length then
        Some(finalMap, state')
      else
        None
