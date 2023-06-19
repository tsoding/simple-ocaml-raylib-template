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

module Vector2 = struct
  type t = { x: float; y: float }
  let add (a: t) (b: t): t =
    { x = a.x +. b.x
    ; y = a.y +. b.y
    }
  let scale (s: float) (a: t): t =
    { x = a.x*.s
    ; y = a.y*.s
    }
  let zero: t = {x = 0.; y = 0.}
end

module Rectangle = struct
  type t = { x: float; y: float; width: float; height: float }
end

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
external draw_circle : int -> int -> float -> color -> unit = "caml_draw_circle"
external draw_rectangle_pro : Rectangle.t -> Vector2.t -> float -> color -> unit = "caml_draw_rectangle_pro"

external is_key_pressed: int -> bool = "caml_is_key_pressed"
external is_key_down: int -> bool = "caml_is_key_down"

external get_mouse_x: unit -> int = "caml_get_mouse_x"
external get_mouse_y: unit -> int = "caml_get_mouse_y"

external is_mouse_button_pressed: int -> bool = "caml_is_mouse_button_pressed"
