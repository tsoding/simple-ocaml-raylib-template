open Raylib

let green =
  { r = 0
  ; g = 228
  ; b = 48
  ; a = 255
  }

let blue =
  { r = 0
  ; g = 121
  ; b = 241
  ; a = 255
  }

let player_size = 100./.2.
let player_color = red
let gy = 2000.0
let gx = 000.0
let dampening = 0.75
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
let projectile_lifetime = 1.10
let shockwave_distance = 500.
let shockwave_impact = 2000.0

let fresh (width: int) (height: int): Game.t =
  { x = (float_of_int width)/.2.
  ; y = (float_of_int height)/.2.
  ; dx = 0.
  ; dy = 100.
  ; mx = 0.
  ; projs = []
  }

let spawn_projectile (game: Game.t) (position: Vector2.t) (velocity: Vector2.t): Game.t =
  let new_proj: Game.Projectile.t =
    { x = position.x
    ; y = position.y
    ; dx = velocity.x
    ; dy = velocity.y
    ; lifetime = projectile_lifetime
    }
  in
  { game with projs = new_proj :: game.projs }

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
  let mouse_x = get_mouse_x () |> float_of_int in
  let mouse_y = get_mouse_y () |> float_of_int in
  let width = get_render_width () |> float_of_int in
  let height = get_render_height () |> float_of_int in
  let tank_position: Vector2.t =
    { x = game.x
    ; y = game.y
    }
  in
  let dome_center: Vector2.t =
    { x = tank_position.x
    ; y = tank_position.y -. tank_height
    }
  in
  let gun_rotation_rads =
    let vx = mouse_x -. dome_center.x in
    let vy = mouse_y -. dome_center.y in
    atan2 vy vx
  in
  let gun_rotation_degrees = gun_rotation_rads/.Float.pi*.180.0 in
  let game =
    if ' ' |> Char.code |> is_key_pressed then
      { game with dy = game.dy -. jump_y
      }
    else if 'Q' |> Char.code |> is_key_pressed then
      fresh (get_render_width ()) (get_render_height ())
    else
      game
  in
  let game =
    if is_mouse_button_pressed 0 then
      let gun_dir: Vector2.t =
        { x = cos gun_rotation_rads
        ; y = sin gun_rotation_rads
        }
      in
      let gun_tip: Vector2.t =
        gun_dir |> Vector2.scale gun_length |> Vector2.add dome_center
      in
      let proj_vel: Vector2.t =
        gun_dir |> Vector2.scale projectile_speed
      in
      spawn_projectile game gun_tip proj_vel
    else game
  in
  let game =
    if 'A' |> Char.code |> is_key_down then
      { game with mx = -.move_speed }
    else
      { game with mx = 0. }
  in
  let game =
    if 'D' |> Char.code |> is_key_down then
      { game with mx = game.mx +. move_speed }
    else game
  in
  let game = { game with dx = game.dx +. gx*.dt
                       ; dy = game.dy +. gy*.dt } in
  let nx = game.x +. (game.dx +. game.mx)*.dt in
  let ny = game.y +. game.dy*.dt in
  let game =
    if nx -. tank_width/.2. < 0. || nx +. tank_width/.2. >= width
    then { game with dx = -.dampening*.game.dx }
    else { game with x = nx }
  in
  let game =
    if ny -. tank_height < 0.
    then { game with dy = -.dampening*.game.dy }
    else { game with y = ny }
  in
  let game =
    if ny >= height
    then { game with y = height
                   ; dy = 0.
                   ; dx = friction*.game.dx }
    else game
  in
  let update_proj (proj: Game.Projectile.t) =
    let proj = { proj with dx = proj.dx +. gx*.dt
                         ; dy = proj.dy +. gy*.dt }
    in
    let nx = proj.x +. proj.dx*.dt in
    let ny = proj.y +. proj.dy*.dt in
    let proj =
      if nx -. projectile_radius < 0. || nx +. projectile_radius >= width
      then { proj with dx = -.dampening*.proj.dx }
      else { proj with x = nx }
    in
    let proj =
      if ny -. projectile_radius < 0. || ny +. projectile_radius >= height
      then { proj with dy = -.dampening*.proj.dy }
      else { proj with y = ny }
    in
    let proj = { proj with lifetime = proj.lifetime-.dt }
    in
    proj
  in
  let new_projs = game.projs |> List.map update_proj in
  let alive_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime > 0.) in
  let dead_projs = new_projs |> List.filter (fun (proj: Game.Projectile.t) -> proj.lifetime <= 0.) in
  let game = { game with projs = alive_projs} in
  let f (proj: Game.Projectile.t): Vector2.t =
    let dx = dome_center.x -. proj.x in
    let dy = dome_center.y -. proj.y in
    let d = sqrt (dx*.dx +. dy*.dy) in
    if d > shockwave_distance then
      { x = 0.; y = 0.}
    else
      let s = 1. -. d/.shockwave_distance in
      let normal: Vector2.t =
        { x = dx/.(d +. 0.001)
        ; y = dy/.(d +. 0.001)
        }
      in
      normal |> Vector2.scale (shockwave_impact *. s)
  in
  let force = dead_projs
              |> List.map f
              |> List.fold_left Vector2.add Vector2.zero
  in
  let game = {game with dx = game.dx +. force.x
                      ; dy = game.dy +. force.y
             }
  in
  begin_drawing ();
  let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
  clear_background background;
  render_tank tank_position gun_rotation_degrees;
  game.projs
  |> List.iter (fun (proj: Game.Projectile.t) ->
         let x = proj.x |> int_of_float in
         let y = proj.y |> int_of_float in
         draw_circle x y projectile_radius projectile_color);
  end_drawing ();
  game

let () =
  (* Registering hot reloaded functions *)
  Game.plug.fresh <- fresh;
  Game.plug.update <- update
