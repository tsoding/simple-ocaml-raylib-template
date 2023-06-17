module Game = struct
  type t =
    { x: float
    ; y: float
    }

  let fresh = { x = 0.; y = 0.; }

  let update (game: t) (dt: float): t =
    game

  let render (game: t) =
    let open Raylib in
    begin_drawing ();
    let background = { r = 0x18; g = 0x18; b = 0x18; a = 0xFF } in
    clear_background background;
    let width = get_render_width () in
    let height = get_render_height () in
    let rect_width = width / 4 in
    let rect_height = height / 4 in
    let rect_x = (width/2 - rect_width/2) in
    let rect_y = (height/2 - rect_height/2) in
    draw_rectangle rect_x rect_y rect_width rect_height red;
    end_drawing ()
end

let () =
  let open Raylib in
  let width = 800 in
  let height = 600 in
  let title = "Game" in
  init_window width height title;
  set_target_fps 60;
  let rec loop (game: Game.t) =
    match window_should_close () with
    | true -> ()
    | false ->
       let dt = 0.016 in
       let game = Game.update game dt in
       Game.render game;
       loop game
  in loop Game.fresh;
  close_window()
