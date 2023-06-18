open Raylib

let green =
  { r = 0
  ; g = 228
  ; b = 48
  ; a = 255
  }

let player_size = 100.
let player_color = red
let gy = 2000.0
let gx = 000.0
let dampness = 0.75
let jump_y = 500.0

let fresh (): Game.t =
  { x = 0.
  ; y = 0.
  ; dx = 100.
  ; dy = 100.
  }

let update (game: Game.t) (dt: float): Game.t =
  let game =
    if ' ' |> Char.code |> is_key_pressed then
      { game with dy = game.dy -. jump_y
      }
    else
      game
  in
  let width = get_render_width () |> float_of_int in
  let height = get_render_height () |> float_of_int in
  let game = { game with dx = game.dx +. gx*.dt
                       ; dy = game.dy +. gy*.dt } in
  let nx = game.x +. game.dx*.dt in
  let ny = game.y +. game.dy*.dt in
  let game =
    if nx < 0. || nx +. player_size >= width
    then { game with dx = -.dampness*.game.dx }
    else { game with x = nx }
  in
  let game =
    if ny < 0. || ny +. player_size >= height
    then { game with dy = -.dampness*.game.dy }
    else { game with y = ny }
  in
  game

let render (game: Game.t) =
  begin_drawing ();
  let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
  clear_background background;
  let x = int_of_float game.x in
  let y = int_of_float game.y in
  let w = int_of_float player_size in
  let h = w in
  draw_rectangle x y w h player_color;
  end_drawing ()

let () =
  Game.plug.fresh <- fresh;
  Game.plug.update <- update;
  Game.plug.render <- render
