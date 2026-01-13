namespace FsBump.Core

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish
open Mibo.Elmish.Graphics3D
open Mibo.Elmish.Graphics2D
open Mibo.Elmish.Culling
open Mibo.Input

module Program =

  // ─────────────────────────────────────────────────────────────
  // Model
  // ─────────────────────────────────────────────────────────────

  type Model = {
    Player: PlayerModel
    Map: Tile list
    PathState: PathState
    ModelStore: IModelStore
    Camera: Camera.State
    Rng: Random
    Skybox: Skybox.State
    SkyboxEffect: Effect
    TouchState: TouchLogic.State
  }

  let mergeInput
    (a: ActionState<PlayerAction>)
    (b: ActionState<PlayerAction>)
    : ActionState<PlayerAction> =
    {
      a with
          Held = Set.union a.Held b.Held
          Started = Set.union a.Started b.Started
          Released = Set.union a.Released b.Released
    }

  // ─────────────────────────────────────────────────────────────
  // Messages
  // ─────────────────────────────────────────────────────────────

  type Msg =
    | Tick of GameTime
    | PlayerMsg of Player.Msg
    | InputChanged of ActionState<PlayerAction>
    | GenerateMap

  // ─────────────────────────────────────────────────────────────
  // Init
  // ─────────────────────────────────────────────────────────────

  let init(ctx: GameContext) : struct (Model * Cmd<Msg>) =
    let modelStore = ModelStore.create ctx
    Assets.load modelStore

    let skyEffect = Assets.skyboxEffect ctx
    let skyState = Skybox.init()

    let rng = Random.Shared

    let sSize, sOffset, sAsset =
      TileBuilder.getAssetData "platform_4x4x1" 0 modelStore

    let startPlatform = {
      Type = TileType.Platform
      Collision = CollisionType.Solid
      Position = Vector3.Zero
      Rotation = 0.0f
      Variant = 0
      Size = sSize
      Style = 0
      AssetName = sAsset
      VisualOffset = sOffset
    }

    let genConfig = {
      MaxJumpHeight = 2.8f
      MaxJumpDistance = 7.0f
      SafetyBuffer = 0.1f
    }

    let initialPath = {
      MapGenerator.createInitialState() with
          Position = Vector3(0.0f, 0.0f, -2.0f)
    }

    let t1, st1 =
      MapGenerator.generateSegment
        rng
        initialPath
        [ startPlatform ]
        genConfig
        modelStore

    let t2, st2 =
      MapGenerator.generateSegment
        rng
        st1
        (startPlatform :: t1)
        genConfig
        modelStore

    let t3, st3 =
      MapGenerator.generateSegment
        rng
        st2
        (startPlatform :: t1 @ t2)
        genConfig
        modelStore

    let spawnVec = MapGenerator.getSpawnPoint()
    let player, pCmd = Player.init spawnVec

    let vp = ctx.GraphicsDevice.Viewport
    let screenSize = Vector2(float32 vp.Width, float32 vp.Height)

    {
      Player = player
      Map = [ startPlatform ] @ t1 @ t2 @ t3
      PathState = st3
      ModelStore = modelStore
      Camera = {
        Position = spawnVec + Vector3(0.0f, 10.0f, 10.0f)
        Target = spawnVec
      }
      Rng = rng
      Skybox = skyState
      SkyboxEffect = skyEffect
      TouchState = TouchLogic.init screenSize
    },
    Cmd.map PlayerMsg pCmd

  // ─────────────────────────────────────────────────────────────
  // Update
  // ─────────────────────────────────────────────────────────────

  let update (msg: Msg) (model: Model) : struct (Model * Cmd<Msg>) =
    match msg with
    | InputChanged input ->
      {
        model with
            Player = { model.Player with Input = input }
      },
      Cmd.none
    | GenerateMap ->
      let result =
        MapGenerator.Operations.updateMap
          model.Rng
          model.ModelStore
          model.Player.Body.Position
          model.Map
          model.PathState

      match result with
      | Some(map', path') ->
        {
          model with
              Map = map'
              PathState = path'
        },
        Cmd.none
      | None -> model, Cmd.none
    | Tick gt ->
      let dt = float32 gt.ElapsedGameTime.TotalSeconds

      // Update touch logic
      let touchState' =
        TouchLogic.update model.TouchState.ScreenSize model.TouchState

      let touchInput = TouchLogic.toActionState touchState'
      let mergedInput = mergeInput model.Player.Input touchInput

      let player' =
        Player.Operations.updateTick dt model.ModelStore model.Map {
          model.Player with
              Input = mergedInput
        }

      let camera' = Camera.update dt player'.Body.Position model.Camera
      let sky' = Skybox.update dt model.Skybox

      let genCmd =
        if
          MapGenerator.Operations.needsUpdate
            player'.Body.Position
            model.PathState
        then
          Cmd.deferNextFrame(Cmd.ofMsg GenerateMap)
        else
          Cmd.none

      {
        model with
            Player = player'
            Camera = camera'
            Skybox = sky'
            TouchState = touchState'
      },
      genCmd
    | PlayerMsg pMsg ->
      let nextPlayer, pCmd = Player.update pMsg model.Player
      { model with Player = nextPlayer }, Cmd.map PlayerMsg pCmd

  // ─────────────────────────────────────────────────────────────
  // View
  // ─────────────────────────────────────────────────────────────

  let viewUI
    (ctx: GameContext)
    (model: Model)
    (buffer: Mibo.Elmish.RenderBuffer<int<RenderLayer>, RenderCmd2D>)
    =
    let vp = ctx.GraphicsDevice.Viewport
    let screenSize = Vector2(float32 vp.Width, float32 vp.Height)
    TouchUI.draw model.ModelStore screenSize model.TouchState buffer

  let view
    (ctx: GameContext)
    (model: Model)
    (buffer: Mibo.Elmish.RenderBuffer<unit, RenderCmd3D>)
    =
    let camera =
      Camera3D.lookAt
        model.Camera.Position
        model.Camera.Target
        Vector3.Up
        (MathHelper.ToRadians 45.f)
        (800.f / 600.f)
        0.1f
        2000.f

    Draw3D.camera camera buffer

    // Draw skybox first
    Skybox.draw
      model.ModelStore
      model.Camera.Position
      model.SkyboxEffect
      model.Skybox
      buffer

    let frustum = Camera3D.boundingFrustum camera

    for tile in model.Map do
      let halfSize = tile.Size * 0.5f
      let box = BoundingBox(tile.Position - halfSize, tile.Position + halfSize)

      if Culling.isGenericVisible frustum box then
        Assets.getAsset tile
        |> model.ModelStore.Get
        |> Option.iter(fun mesh ->
          let world =
            Matrix.CreateTranslation(tile.VisualOffset)
            * Matrix.CreateRotationY(tile.Rotation)
            * Matrix.CreateTranslation(tile.Position)

          Draw3D.mesh mesh world |> Draw3D.submit buffer)

    model.ModelStore.Get Assets.PlayerBall
    |> Option.iter(fun mesh ->
      let world =
        Matrix.CreateScale(model.Player.Body.Radius * 2.0f)
        * Matrix.CreateFromQuaternion(model.Player.Rotation)
        * Matrix.CreateTranslation(model.Player.Body.Position)

      Draw3D.mesh mesh world
      |> Draw3D.withColor Color.White
      |> Draw3D.submit buffer)

  let create() =
    Program.mkProgram init update
    |> Program.withAssets
    |> Program.withRenderer(
      Batch3DRenderer.createWithConfig
        {
          Batch3DConfig.defaults with
              ClearColor = ValueSome Color.CornflowerBlue
        }
        view
    )
    |> Program.withRenderer(Batch2DRenderer.create viewUI)
    |> Program.withInput
    |> Program.withSubscription(fun ctx _ ->
      InputMapper.subscribeStatic
        Player.Input.config
        (fun input -> InputChanged input)
        ctx)
    |> Program.withTick Tick
    |> Program.withConfig(fun (game, graphics) ->
      game.Content.RootDirectory <- "Content"
      game.Window.Title <- "Procedural Map"
      game.IsMouseVisible <- true
      graphics.PreferMultiSampling <- true
      graphics.SynchronizeWithVerticalRetrace <- true

      graphics.PreparingDeviceSettings.Add(fun e ->
        let pp = e.GraphicsDeviceInformation.PresentationParameters
        pp.MultiSampleCount <- 8))
