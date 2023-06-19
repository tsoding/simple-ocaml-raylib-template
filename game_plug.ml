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
let jump_y = 1000.0
let gun_length = 120.
let gun_girth = 20.
let tank_width = 150.
let tank_height = 80.
let dome_radius = 50.
let move_speed = 500.

let fresh (width: int) (height: int): Game.t =
  { x = (float_of_int width)/.2.
  ; y = (float_of_int height)/.2.
  ; dx = 200.
  ; dy = 100.
  ; mx = 0.
  }

let update (game: Game.t) (dt: float): Game.t =
  let width = get_render_width () |> float_of_int in
  let height = get_render_height () |> float_of_int in
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
                   ; dx = 0. }
    else game
  in
  game

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

let render (game: Game.t) =
  begin_drawing ();
  let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
  clear_background background;
  let position: Vector2.t =
    { x = game.x
    ; y = game.y
    }
  in
  let dome_center: Vector2.t =
    { x = position.x
    ; y = position.y -. tank_height
    }
  in
  let mouse_x = get_mouse_x () |> float_of_int in
  let mouse_y = get_mouse_y () |> float_of_int in
  let vx = mouse_x -. dome_center.x in
  let vy = mouse_y -. dome_center.y in
  let rads = atan2 vy vx in
  let degrees = rads/.Float.pi*.180.0 in
  render_tank position degrees;
  draw_circle (get_mouse_x ()) (get_mouse_y ()) 15. green;
  end_drawing ()

let () =
  (* Registering hot reloaded functions *)
  Game.plug.fresh <- fresh;
  Game.plug.update <- update;
  Game.plug.render <- render
