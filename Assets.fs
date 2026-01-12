namespace ProceduralMap

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Assets =
  open Mibo.Elmish
  let PlayerBall = "kaykit_platformer/blue/ball_blue"

  let private colors = [ "blue"; "green"; "red"; "yellow" ]

  let private baseAssets = [
    "platform_1x1x1"
    "platform_2x2x1"
    "platform_2x2x2"
    "platform_2x2x4"
    "platform_4x2x1"
    "platform_4x2x2"
    "platform_4x2x4"
    "platform_4x4x1"
    "platform_4x4x2"
    "platform_4x4x4"
    "platform_6x2x1"
    "platform_6x2x2"
    "platform_6x2x4"
    "platform_6x6x1"
    "platform_6x6x2"
    "platform_6x6x4"
    "platform_decorative_1x1x1"
    "platform_decorative_2x2x2"
    "platform_slope_2x2x2"
    "platform_slope_2x4x4"
    "platform_slope_2x6x4"
    "platform_slope_4x2x2"
    "platform_slope_4x4x4"
    "platform_slope_4x6x4"
    "platform_slope_6x2x2"
    "platform_slope_6x4x4"
    "platform_slope_6x6x4"
    "barrier_1x1x1"
    "barrier_1x1x2"
    "barrier_1x1x4"
    "barrier_2x1x1"
    "barrier_2x1x2"
    "barrier_2x1x4"
    "barrier_3x1x1"
    "barrier_3x1x2"
    "barrier_3x1x4"
    "barrier_4x1x1"
    "barrier_4x1x2"
    "barrier_4x1x4"
    "arch"
    "arch_tall"
    "arch_wide"
    "bomb_A"
    "bomb_B"
    "bracing_large"
    "bracing_medium"
    "bracing_small"
    "button_base"
    "cone"
    "diamond"
    "heart"
    "star"
    "flag_A"
    "flag_B"
    "flag_C"
    "hoop"
    "hoop_angled"
    "lever_floor_base"
    "lever_wall_base_A"
    "lever_wall_base_B"
    "pipe_180_A"
    "pipe_180_B"
    "pipe_90_A"
    "pipe_90_B"
    "pipe_end"
    "pipe_straight_A"
    "pipe_straight_B"
    "power"
    "signage_arrow_stand"
    "signage_arrow_wall"
    "spring_pad"
  ]

  let load(modelStore: IModelStore) =
    for c in colors do
      for a in baseAssets do
        modelStore.Load(sprintf "kaykit_platformer/%s/%s_%s" c a c)

    modelStore.Load PlayerBall
    modelStore.Load "kaykit_platformer/neutral/pillar_1x1x2"
    modelStore.Load "cube"

  let skyboxEffect(ctx: Mibo.Elmish.GameContext) =
    Assets.effect "skyboxes/NightSky" ctx

  let private getColor variant =
    match variant % 4 with
    | 0 -> "blue"
    | 1 -> "green"
    | 2 -> "red"
    | _ -> "yellow"

  let getAsset(tile: Tile) =
    if not(String.IsNullOrEmpty tile.AssetName) then
      let color = getColor tile.Variant
      sprintf "kaykit_platformer/%s/%s_%s" color tile.AssetName color
    else
      let color = getColor tile.Variant

      match tile.Type with
      | TileType.Floor ->
        sprintf "kaykit_platformer/%s/platform_1x1x1_%s" color color
      | _ -> ""
