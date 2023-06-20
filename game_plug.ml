open Raylib

let player_size = 100./.2.
let player_color = red
let gravity = Vector2.vec2 0. 2000.
let dumpling = 0.75
let friction = 0.9
let jump_y = 1000.0
let gun_length = 120.
let gun_girth = 20.
let tank_width = 150.
let tank_height = 80.
let dome_radius = 50.
let move_speed = 250.
let projectile_speed = 1000.0
let projectile_radius = gun_girth/.2.
let projectile_color = green
let projectile_lifetime = 2.
let shockwave_distance = 500.
let shockwave_impact = 2000.0
let shockwave_color = green
let explosion_duration = 0.25

let fresh (width: int) (height: int): Game.t =
  let w = float_of_int width in
  let h = float_of_int height in
  let open Vector2 in
  { pos = vec2 w h /^ scalar 2.
  ; vel = vec2 0. 100.
  ; mov = scalar 0.
  ; projs = []
  ; explosions = []
  }

let spawn_projectile (game: Game.t) (pos: Vector2.t) (vel: Vector2.t): Game.t =
  let new_proj: Game.Projectile.t =
    { pos
    ; vel
    ; lifetime = projectile_lifetime
    }
  in
  { game with projs = new_proj :: game.projs }

let spawn_explosion (game: Game.t) (pos: Vector2.t): Game.t =
  let new_explosion: Game.Explosion.t = { pos; progress = 1. } in
  { game with explosions = new_explosion :: game.explosions }

let render_gun (pivot: Vector2.t) (rotation: float): unit =
  let rectangle: Rectangle.t =
    { x = pivot.x
    ; y = pivot.y
    ; width = gun_length
    ; height = gun_girth
    }
  in
  let origin: Vector2.t = { x = 0.; y = gun_girth/.2. } in
  draw_rectangle_pro rectangle origin rotation red

let render_tank (position: Vector2.t) (rotation: float): unit =
  draw_rectangle
    (int_of_float (position.x -. tank_width/.2.))
    (int_of_float (position.y -. tank_height))
    (int_of_float tank_width)
    (int_of_float tank_height)
    red;
  let dome_center: Vector2.t =
    { x = position.x
    ; y = position.y -. tank_height
    }
  in
  draw_circle
    (int_of_float dome_center.x)
    (int_of_float dome_center.y)
    dome_radius
    red;
  render_gun dome_center rotation

let update (dt: float) (game: Game.t): Game.t =
  let open Vector2 in

  (* Computable parameters of the game *)
  let mouse = ivec2 (get_mouse_x ()) (get_mouse_y ()) in
  let res = ivec2 (get_render_width ()) (get_render_height ()) in
  let dome_center = game.pos -^ vec2 0. tank_height in
  let gun_rotation_rads = dir (mouse -^ dome_center) in
  let gun_rotation_degrees = gun_rotation_rads/.Float.pi*.180.0 in

  (* Tank Controls *)
  let game =
    (* Reset the state of the game *)
    let game =
      if 'Q' |> Char.code |> is_key_pressed
      then fresh (get_render_width ()) (get_render_height ())
      else game
    in

    (* Shooting projectile *)
    let game =
      if is_mouse_button_pressed 0 then
        let gun_dir: Vector2.t =
          { x = cos gun_rotation_rads
          ; y = sin gun_rotation_rads
          }
        in
        let open Vector2 in
        let gun_tip: Vector2.t =
          gun_dir *^ scalar gun_length +^ dome_center
        in
        let proj_vel: Vector2.t =
          gun_dir *^ scalar projectile_speed
        in
        spawn_projectile game gun_tip proj_vel
      else game
    in

    (* Moving left *)
    let game =
      if 'A' |> Char.code |> is_key_down then
        { game with mov = vec2 (-.move_speed) 0. }
      else
        { game with mov = scalar 0. }
    in

    (* Moving right *)
    let game =
      if 'D' |> Char.code |> is_key_down then
        { game with mov = game.mov +^ vec2 move_speed 0. }
      else game
    in
    game
  in

  (* Tank Physics *)
  let game =
    let game =
      { game with vel = game.vel +^ gravity*^scalar dt }
    in

    let game =
      { game with pos = game.pos +^ (game.vel +^ game.mov)*^scalar dt }
    in

    (* Bottom collision *)
    let game =
      if game.pos.y >= res.y
      then { game with pos = { game.pos with y = res.y }
                     ; vel = game.vel *^ vec2 friction 0.
           }
      else game
    in

    game
  in

  let update_proj (proj: Game.Projectile.t): Game.Projectile.t =
    let open Vector2 in
    let proj = { proj with vel = proj.vel +^ gravity*^scalar dt } in
    let proj = { proj with pos = proj.pos +^ proj.vel*^scalar dt } in
    let proj = { proj with lifetime = if proj.pos.y +. projectile_radius >= res.y
                                      then 0.
                                      else proj.lifetime -. dt }
    in
    proj
  in
  let new_projs = game.projs |> List.map update_proj in
  let alive_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime > 0.) in
  let dead_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime <= 0.) in
  let game = { game with projs = alive_projs } in
  let exploded_projectile_force (proj: Game.Projectile.t): Vector2.t =
    let open Vector2 in
    let direction = dome_center -^ proj.pos in
    let distance = mag direction in
    if distance > shockwave_distance then
      { x = 0.; y = 0.}
    else
      let s = 1. -. distance/.shockwave_distance in
      let eps = 0.001 in        (* to prevent the division by distance == 0 *)
      direction /^ scalar (distance +. eps) *^ scalar (shockwave_impact *. s)
  in
  let force =
    let open Vector2 in
    dead_projs
    |> List.map exploded_projectile_force
    |> List.fold_left (+^) (scalar 0.)
  in
  let game = { game with vel = game.vel +^ force } in

  let game =
    let explosion_from_proj (proj: Game.Projectile.t): Game.Explosion.t =
      { pos = proj.pos
      ; progress = 1.
      }
    in
    let update_explosion (explosion: Game.Explosion.t): Game.Explosion.t =
      { explosion with progress = (explosion_duration*.explosion.progress -. dt)/.explosion_duration }
    in
    let is_explosion_alive (explosion: Game.Explosion.t): bool =
      explosion.progress > 0.
    in
    let new_explosions = dead_projs |> List.map explosion_from_proj in
    { game with explosions =
                  game.explosions
                  |> List.map update_explosion
                  |> List.filter is_explosion_alive
                  |> List.append new_explosions
    }
  in

  (* Rendering the state of the game *)
  begin_drawing ();
  let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
  clear_background background;
  game.projs
  |> List.iter (fun (proj: Game.Projectile.t) ->
         let x = proj.pos.x |> int_of_float in
         let y = proj.pos.y |> int_of_float in
         draw_circle x y projectile_radius projectile_color);
  game.explosions
  |> List.iter (fun (explosion: Game.Explosion.t) ->
       let x = explosion.pos.x |> int_of_float in
       let y = explosion.pos.y |> int_of_float in
       let r = shockwave_distance*.0.5*.explosion.progress in
       draw_circle x y r shockwave_color);
  render_tank game.pos gun_rotation_degrees;
  end_drawing ();
  game

let () =
  (* Registering hot reloaded functions *)
  Game.plug.fresh <- fresh;
  Game.plug.update <- update
