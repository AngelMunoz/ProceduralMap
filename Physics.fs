namespace ProceduralMap

open System
open Microsoft.Xna.Framework

module Physics =

  module Constants =
    let Gravity = -40.0f
    let JumpSpeed = 15.0f
    let StepHeight = 0.5f
    let SlopeSnapDistance = 1.0f
    let MoveSpeed = 10.0f
    let Acceleration = 30.0f
    let Friction = 20.0f
    let RollSpeed = 2.0f

  module private Math =
    /// Bounding box intersection check for a sphere
    let sphereAABB
      (spherePos: Vector3)
      (radius: float32)
      (boxMin: Vector3)
      (boxMax: Vector3)
      =
      let x = Math.Clamp(spherePos.X, boxMin.X, boxMax.X)
      let y = Math.Clamp(spherePos.Y, boxMin.Y, boxMax.Y)
      let z = Math.Clamp(spherePos.Z, boxMin.Z, boxMax.Z)
      let closest = Vector3(x, y, z)
      Vector3.DistanceSquared(spherePos, closest) < (radius * radius)

    /// Finds the closest point on a triangle (ABC) to a given point (P)
    let closestPointOnTriangle
      (p: Vector3)
      (a: Vector3)
      (b: Vector3)
      (c: Vector3)
      =
      let ab, ac, ap = b - a, c - a, p - a
      let d1, d2 = Vector3.Dot(ab, ap), Vector3.Dot(ac, ap)

      if d1 <= 0.0f && d2 <= 0.0f then
        a
      else
        let bp = p - b
        let d3, d4 = Vector3.Dot(ab, bp), Vector3.Dot(ac, bp)

        if d3 >= 0.0f && d4 <= d3 then
          b
        else
          let vc = d1 * d4 - d3 * d2

          if vc <= 0.0f && d1 >= 0.0f && d3 <= 0.0f then
            a + ab * (d1 / (d1 - d3))
          else
            let cp = p - c
            let d5, d6 = Vector3.Dot(ab, cp), Vector3.Dot(ac, cp)

            if d6 >= 0.0f && d5 <= d6 then
              c
            else
              let vb = d5 * d2 - d1 * d6

              if vb <= 0.0f && d2 >= 0.0f && d6 <= 0.0f then
                a + ac * (d2 / (d2 - d6))
              else
                let va = d3 * d6 - d5 * d4

                if va <= 0.0f && (d4 - d3) >= 0.0f && (d5 - d6) >= 0.0f then
                  b + (c - b) * ((d4 - d3) / ((d4 - d3) + (d5 - d6)))
                else
                  let denom = 1.0f / (va + vb + vc)
                  a + ab * (vb * denom) + ac * (vc * denom)

  module Mesh =
    /// Maps a tile to its full asset path
    let getAssetId(tile: Tile) =
      let color =
        match tile.Variant % 4 with
        | 0 -> "blue"
        | 1 -> "green"
        | 2 -> "red"
        | _ -> "yellow"

      if String.IsNullOrEmpty tile.AssetName then
        ""
      elif tile.AssetName.Contains("/") then
        tile.AssetName
      else
        sprintf "kaykit_platformer/%s/%s_%s" color tile.AssetName color

    /// Resolves collision against mesh geometry
    let resolve (body: Body) (tile: Tile) (modelStore: IModelStore) =
      modelStore.GetGeometry(getAssetId tile)
      |> Option.bind(fun geo ->
        let world =
          Matrix.CreateTranslation(tile.VisualOffset)
          * Matrix.CreateRotationY(tile.Rotation)
          * Matrix.CreateTranslation(tile.Position)

        let mutable p, v, hit = body.Position, body.Velocity, false
        let half = tile.Size * 0.5f
        let min, max = tile.Position - half, tile.Position + half

        if Math.sphereAABB p body.Radius min max then
          for i in 0..3 .. geo.Indices.Length - 3 do
            let a = Vector3.Transform(geo.Vertices.[geo.Indices.[i]], world)

            let b =
              Vector3.Transform(geo.Vertices.[geo.Indices.[i + 1]], world)

            let c =
              Vector3.Transform(geo.Vertices.[geo.Indices.[i + 2]], world)

            let closest = Math.closestPointOnTriangle p a b c
            let distSq = Vector3.DistanceSquared(p, closest)

            if distSq < (body.Radius * body.Radius) then
              let dist = sqrt distSq

              let normal =
                if dist < 0.0001f then
                  Vector3.Normalize(Vector3.Cross(b - a, c - a))
                else
                  (p - closest) / dist

              p <- p + normal * (body.Radius - dist)
              let dot = Vector3.Dot(v, normal)

              if dot < 0.0f then
                v <- v - normal * dot

              hit <- true

        if hit then Some(p, v) else None)

  module private Steps =
    /// Resolve vertical movement and collisions
    let verticalPass
      dt
      (tiles: Tile list)
      (modelStore: IModelStore)
      (body: Body, grounded: bool)
      =
      let mutable pos = body.Position + Vector3.Up * body.Velocity.Y * dt
      let mutable vel = body.Velocity
      let mutable groundedNow = false

      for t in tiles do
        let currentBody = {
          body with
              Position = pos
              Velocity = vel
        }

        match t.Type with
        | TileType.SlopeTile
        | TileType.Decoration ->
          Mesh.resolve currentBody t modelStore
          |> Option.iter(fun (np, nv) ->
            let normal = Vector3.Normalize(np - pos)

            if normal.Y > 0.5f then
              groundedNow <- true

            pos <- np
            vel <- nv)
        | _ ->
          match t.Collision with
          | CollisionType.Solid
          | CollisionType.Climbable ->
            let half = t.Size * 0.5f
            let min, max = t.Position - half, t.Position + half

            if Math.sphereAABB pos body.Radius min max then
              if vel.Y < 0.0f && pos.Y > min.Y then
                pos <- Vector3(pos.X, max.Y + body.Radius, pos.Z)
                vel <- Vector3(vel.X, 0.0f, vel.Z)
                groundedNow <- true
              elif vel.Y > 0.0f && pos.Y < max.Y then
                pos <- Vector3(pos.X, min.Y - body.Radius, pos.Z)
                vel <- Vector3(vel.X, 0.0f, vel.Z)
          | _ -> ()

      {
        body with
            Position = pos
            Velocity = vel
      },
      groundedNow

    /// Resolve horizontal movement and collisions
    let horizontalPass
      (isX: bool)
      (dt: float32)
      (tiles: Tile list)
      (modelStore: IModelStore)
      (body: Body, grounded: bool)
      =
      let axis = if isX then Vector3.UnitX else Vector3.UnitZ
      let moveAmount = (if isX then body.Velocity.X else body.Velocity.Z) * dt

      if abs moveAmount < 0.00001f then
        body, grounded
      else
        let nextPos = body.Position + axis * moveAmount

        let mutable collided, groundedNow, finalPos, finalVel =
          false, grounded, nextPos, body.Velocity

        for t in tiles do
          match t.Type with
          | TileType.SlopeTile
          | TileType.Decoration ->
            Mesh.resolve { body with Position = nextPos } t modelStore
            |> Option.iter(fun (np, nv) ->
              let normal = Vector3.Normalize(np - nextPos)
              // If normal points significantly along the axis, it's a horizontal collision
              if abs(if isX then normal.X else normal.Z) > 0.5f then
                collided <- true
              // If normal points significantly UP, it's a step/slope surface
              if normal.Y > 0.5f then
                finalPos <- np
                finalVel <- nv
                groundedNow <- true)
          | _ ->
            match t.Collision with
            | CollisionType.Solid
            | CollisionType.Climbable ->
              let half = t.Size * 0.5f
              let min, max = t.Position - half, t.Position + half
              let wallMax = Vector3(max.X, max.Y - 0.1f, max.Z)

              if Math.sphereAABB nextPos body.Radius min wallMax then
                let canStep =
                  t.Collision = CollisionType.Climbable
                  || (max.Y
                      <= body.Position.Y - body.Radius + Constants.StepHeight)

                if canStep then
                  finalPos <-
                    Vector3(finalPos.X, max.Y + body.Radius, finalPos.Z)

                  groundedNow <- true
                else
                  collided <- true
            | _ -> ()

        let vel =
          if collided then
            (if isX then
               Vector3(0.0f, finalVel.Y, finalVel.Z)
             else
               Vector3(finalVel.X, finalVel.Y, 0.0f))
          else
            finalVel

        let pos = if collided then body.Position else finalPos

        {
          body with
              Position = pos
              Velocity = vel
        },
        groundedNow

  let updateBody
    (dt: float32)
    (body: Body)
    (nearbyTiles: Tile list)
    (modelStore: IModelStore)
    (jumpRequested: bool)
    (isGrounded: bool)
    : struct (Body * bool) =
    let mutable v = body.Velocity

    if jumpRequested && isGrounded then
      v.Y <- Constants.JumpSpeed

    v.Y <- v.Y + Constants.Gravity * dt

    let body', grounded' =
      ({ body with Velocity = v }, false)
      |> Steps.verticalPass dt nearbyTiles modelStore
      |> (fun (b, g) ->
        Steps.horizontalPass true dt nearbyTiles modelStore (b, g))
      |> (fun (b, g) ->
        Steps.horizontalPass false dt nearbyTiles modelStore (b, g))

    struct (body', grounded')
