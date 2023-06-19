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
let dampness = 0.75
let jump_y = 1000.0
let gun_length = 120.
let gun_girth = 20.
let tank_width = 150.
let tank_height = 80.
let dome_radius = 50.

let fresh (): Game.t =
  { x = 0.
  ; y = 0.
  ; dx = 100.
  ; dy = 100.
  }

let update (game: Game.t) (dt: float): Game.t =
  let width = get_render_width () |> float_of_int in
  let height = get_render_height () |> float_of_int in
  let game =
    if ' ' |> Char.code |> is_key_pressed then
      { game with dy = game.dy -. jump_y
      }
    else if 'Q' |> Char.code |> is_key_pressed then
      { x = width/.2.
      ; y = height/.2.
      ; dx = 200.
      ; dy = 100.
      }
    else
      game
  in
  let game = { game with dx = game.dx +. gx*.dt
                       ; dy = game.dy +. gy*.dt } in
  let nx = game.x +. game.dx*.dt in
  let ny = game.y +. game.dy*.dt in
  let game =
    if nx -. tank_width/.2. < 0. || nx +. tank_width/.2. >= width
    then { game with dx = -.dampness*.game.dx }
    else { game with x = nx }
  in
  let game =
    if ny -. tank_height < 0. || ny >= height
    then { game with dy = -.dampness*.game.dy }
    else { game with y = ny }
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

let render_tank (position: Vector2.t): unit =
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
  render_gun dome_center (-.0.)

let render (game: Game.t) =
  begin_drawing ();
  let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
  clear_background background;
  let pivot: Vector2.t =
    { x = game.x
    ; y = game.y
    }
  in
  render_tank pivot;
  end_drawing ()

let () =
  (* Registering hot reloaded functions *)
  Game.plug.fresh <- fresh;
  Game.plug.update <- update;
  Game.plug.render <- render
