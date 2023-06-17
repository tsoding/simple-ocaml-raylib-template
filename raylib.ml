type color =
  { r: int
  ; g: int
  ; b: int
  ; a: int
  }

let red: color =
  { r = 230
  ; g = 41
  ; b = 55
  ; a = 255
  }

external init_window: int -> int -> string -> unit = "caml_init_window"
external set_target_fps: int -> unit = "caml_set_target_fps"
external window_should_close: unit -> bool = "caml_window_should_close"
external begin_drawing: unit -> unit = "caml_begin_drawing"
external end_drawing: unit -> unit = "caml_end_drawing"
external clear_background: color -> unit = "caml_clear_background"
external close_window: unit -> unit = "caml_close_window"
external draw_rectangle: int -> int -> int -> int -> color -> unit = "caml_draw_rectangle"
external get_render_width: unit -> int = "caml_get_render_width"
external get_render_height: unit -> int = "caml_get_render_height"
