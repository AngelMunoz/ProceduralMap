namespace ProceduralMap

open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish

module ModelStore =
  open System

  let private extractModelData(model: Model) =
    let mutable min = Vector3(Single.MaxValue)
    let mutable max = Vector3(Single.MinValue)
    let allVertices = ResizeArray<Vector3>()
    let allIndices = ResizeArray<int>()
    let mutable vertexOffset = 0

    for mesh in model.Meshes do
      for part in mesh.MeshParts do
        let declaration = part.VertexBuffer.VertexDeclaration
        let elements = declaration.GetVertexElements()

        let posElement =
          elements
          |> Array.tryFind(fun e ->
            e.VertexElementUsage = VertexElementUsage.Position)

        match posElement with
        | Some elem ->
          // 1. Get Vertices
          let partVertices = Array.zeroCreate<Vector3> part.NumVertices

          part.VertexBuffer.GetData(
            part.VertexOffset * declaration.VertexStride + int elem.Offset,
            partVertices,
            0,
            part.NumVertices,
            declaration.VertexStride
          )

          for v in partVertices do
            min <- Vector3.Min(min, v)
            max <- Vector3.Max(max, v)
            allVertices.Add(v)

          // 2. Get Indices
          if
            part.IndexBuffer.IndexElementSize = IndexElementSize.SixteenBits
          then
            let partIndices = Array.zeroCreate<uint16>(part.PrimitiveCount * 3)

            part.IndexBuffer.GetData(
              part.StartIndex * 2,
              partIndices,
              0,
              part.PrimitiveCount * 3
            )

            for i in partIndices do
              allIndices.Add(int i + vertexOffset)
          else
            let partIndices = Array.zeroCreate<int>(part.PrimitiveCount * 3)

            part.IndexBuffer.GetData(
              part.StartIndex * 4,
              partIndices,
              0,
              part.PrimitiveCount * 3
            )

            for i in partIndices do
              allIndices.Add(i + vertexOffset)

          vertexOffset <- vertexOffset + part.NumVertices
        | None -> ()

    let finalBounds =
      if min.X > max.X then
        BoundingBox(Vector3.Zero, Vector3.One)
      else
        BoundingBox(min, max)

    finalBounds,
    {
      Vertices = allVertices.ToArray()
      Indices = allIndices.ToArray()
    }

  /// Creates a new instance of the ModelStore service.
  let create(ctx: GameContext) =
    let modelCache = Dictionary<string, Model>()
    let boundsCache = Dictionary<string, BoundingBox>()
    let geometryCache = Dictionary<string, ModelGeometry>()

    { new IModelStore with

        member _.Load(assetName: string) =
          if not(modelCache.ContainsKey assetName) then
            try
              let model = Assets.model assetName ctx
              let bounds, geometry = extractModelData model

              modelCache.[assetName] <- model
              boundsCache.[assetName] <- bounds
              geometryCache.[assetName] <- geometry
            with ex ->
              printfn "Failed to load asset '%s': %O" assetName ex

        member _.Get(assetName: string) =
          match modelCache.TryGetValue assetName with
          | true, model -> Some model
          | false, _ -> None

        member _.GetBounds(assetName: string) =
          match boundsCache.TryGetValue assetName with
          | true, bounds -> Some bounds
          | false, _ -> None

        member _.GetGeometry(assetName: string) =
          match geometryCache.TryGetValue assetName with
          | true, geo -> Some geo
          | false, _ -> None
    }
