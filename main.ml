let () =
  let open Raylib in
  let plug_file_path = "game_plug.cmo" in
  Dynlink.loadfile_private plug_file_path;
  let factor = 80 in
  let width = 16*factor in
  let height = 9*factor in
  let title = "Game" in
  init_window width height title;
  set_target_fps 60;
  let rec loop (game: Game.t) =
    match window_should_close () with
    | true -> ()
    | false ->
       if 'R' |> Char.code |> is_key_pressed then
         begin
           Dynlink.loadfile_private plug_file_path;
           Printf.printf "Loaded %s\n" plug_file_path;
           flush stdout
         end;
       let dt = 0.016 in
       game |> Game.plug.update dt |> loop
  in Game.plug.fresh width height |> loop;
  close_window()
