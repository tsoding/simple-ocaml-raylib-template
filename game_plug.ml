open Raylib

let gray = { r = 130; g = 130; b = 130; a = 255}

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
let platform_color = blue
let health_bar_padding = 50.
let health_bar_girth = 20.

let platforms: Rectangle.t list =
  let s = 100. in
  [ { x = 0.; y = 0.; width = s; height = s }
  ; { x = s +. s; y = -. s *. 4.; width = s *. 5.; height = s }
  ; { x = s *. 5.; y = -. s *. 8.; width = s *. 5.; height = s }
  ]

let rectangle_sides (a: Rectangle.t): (float * float * float * float) =
  let l = a.x in
  let r = a.x +. a.width in
  let t = a.y in
  let b = a.y +. a.height in
  l, r, t, b

let rectangle_overlap (a: Rectangle.t) (b: Rectangle.t): bool =
  let l1, r1, t1, b1 = rectangle_sides a in
  let l2, r2, t2, b2 = rectangle_sides b in
  r1 >= l2 && r2 >= l1 && b1 >= t2 && b2 >= t1

let rectangle_contains (p: Vector2.t) (r: Rectangle.t): bool =
  let l, r, t, b = rectangle_sides r in
  l <= p.x && p.x <= r && t <= p.y && p.y <= b

let fresh (): Game.t =
  let open Vector2 in
  { pos = vec2 0. (-.300.)
  ; vel = vec2 0. 100.
  ; mov = scalar 0.
  ; projs = []
  ; explosions = []
  ; health = 1.
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

let tank_hitbox_at (pos: Vector2.t): Rectangle.t =
  { x = pos.x -. tank_width/.2.
  ; y = pos.y -. tank_height
  ; width = tank_width
  ; height = tank_height
  }

let update (dt: float) (game: Game.t): Game.t =
  let open Vector2 in

  (* Computable parameters of the game *)
  let res = ivec2 (get_render_width ()) (get_render_height ()) in

  let dome_center pos =
    pos -^ vec2 0. tank_height
  in

  let camera (pos: Vector2.t) : Camera2D.t =
    { offset = ~^ (pos  -^ res/^scalar 2.)
    ; zoom = 1.
    }
  in

  let mouse_screen = ivec2 (get_mouse_x ()) (get_mouse_y ()) in
  let mouse_world = get_screen_to_world2d mouse_screen (game.pos |> dome_center |> camera) in

  let gun_rotation_rads = dir (mouse_world -^ (dome_center game.pos)) in
  let gun_rotation_degrees = gun_rotation_rads/.Float.pi*.180.0 in

  (* Tank Controls *)
  let game =
    (* Reset the state of the game *)
    let game =
      if 'Q' |> Char.code |> is_key_pressed
      then fresh ()
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
          gun_dir *^ scalar gun_length +^ (dome_center game.pos)
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

    let pos1 = game.pos +^ (game.vel +^ game.mov)*^scalar dt in

    let horz_collis = List.exists (rectangle_overlap (vec2 pos1.x game.pos.y |> tank_hitbox_at)) platforms in
    let game =
      if horz_collis then
        { game with vel = game.vel *^ vec2 0. friction }
      else
        { game with pos = { game.pos with x = pos1.x } }
    in

    let vert_collis = List.exists (rectangle_overlap (vec2 game.pos.x pos1.y |> tank_hitbox_at)) platforms in
    let game =
      if vert_collis then
        { game with vel = game.vel *^ vec2 friction 0. }
      else
        { game with pos = { game.pos with y = pos1.y } }
    in

    (* Bottom collision *)
    let game =
      if game.pos.y >= 0.
      then { game with pos = { game.pos with y = 0. }
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
    let proj = { proj with lifetime = if proj.pos.y >= 0. || (List.exists (rectangle_contains proj.pos) platforms)
                                      then 0.
                                      else proj.lifetime -. dt }
    in
    proj
  in
  let new_projs = game.projs |> List.map update_proj in
  let alive_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime > 0.) in
  let dead_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime <= 0.) in
  let game = { game with projs = alive_projs } in
  let exploded_projectile_force (pos: Vector2.t) (proj: Game.Projectile.t): Vector2.t =
    let open Vector2 in
    let direction = dome_center pos -^ proj.pos in
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
    |> List.map (exploded_projectile_force game.pos)
    |> List.fold_left (+^) (scalar 0.)
  in
  let game = { game with vel = game.vel +^ force
                       ; health = if (mag force) > 1.
                                  then max (game.health -. 0.1) 0.
                                  else game.health
             }
  in
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
    begin_mode_2d (game.pos |> dome_center |> camera);
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
      platforms
      |> List.iter (fun (platform: Rectangle.t) ->
           let x = platform.x |> int_of_float in
           let y = platform.y |> int_of_float in
           let w = platform.width |> int_of_float in
           let h = platform.height |> int_of_float in
           draw_rectangle x y w h platform_color);
      let () =
        let health_pos =
          (game.pos |> dome_center)
          -^ vec2 0. dome_radius
          -^ vec2 0. health_bar_padding
          -^ vec2 (tank_width/.2.) 0.
          -^ vec2 0. (health_bar_girth/.2.)
        in
        let () =
          let x = health_pos.x |> int_of_float in
          let y = health_pos.y |> int_of_float in
          let w = tank_width |> int_of_float in
          let h = health_bar_girth |> int_of_float in
          draw_rectangle x y w h gray
        in ();
        let () =
          let x = health_pos.x |> int_of_float in
          let y = health_pos.y |> int_of_float in
          let w = (tank_width *. game.health) |> int_of_float in
          let h = health_bar_girth |> int_of_float in
          draw_rectangle x y w h red
        in ()
      in ();
    end_mode_2d ();
  end_drawing ();
  game

let () =
  (* Registering hot reloaded functions *)
  Game.plug.fresh <- fresh;
  Game.plug.update <- update
