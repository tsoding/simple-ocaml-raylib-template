type color =
  { r: int
  ; g: int
  ; b: int
  ; a: int
  }

let red = { r = 230 ; g = 41 ; b = 55 ; a = 255 }
let green = { r = 0 ; g = 228 ; b = 48 ; a = 255 }
let blue = { r = 0 ; g = 121 ; b = 241 ; a = 255 }

module Vector2 = struct
  type t = { x: float; y: float }
  let ivec2 x y = { x = float_of_int x; y = float_of_int y }
  let vec2 x y = { x; y }
  let ( ~^ ) (a: t): t =
    { x = -.a.x
    ; y = -.a.y
    }
  let ( -^ ) (a: t) (b: t): t =
    { x = a.x -. b.x
    ; y = a.y -. b.y
    }
  let ( +^ ) (a: t) (b: t): t =
    { x = a.x +. b.x
    ; y = a.y +. b.y
    }
  let ( *^ ) (a: t) (b: t): t =
    { x = a.x *. b.x
    ; y = a.y *. b.y
    }
  let ( /^ ) (a: t) (b: t): t =
    { x = a.x /. b.x
    ; y = a.y /. b.y
    }
  let scalar (s: float): t =
    { x = s; y = s }
  let mag (v: t): float =
    sqrt (v.x*.v.x +. v.y*.v.y)
  let dir (v: t): float =
    atan2 v.y v.x
end

module Camera2D = struct
  type t =
    { offset: Vector2.t
    ; zoom: float
    }
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

external begin_mode_2d: Camera2D.t -> unit = "caml_begin_mode_2d"
external end_mode_2d: unit -> unit = "caml_end_mode_2d"

external get_screen_to_world2d: Vector2.t -> Camera2D.t -> Vector2.t = "caml_get_screen_to_world2d"
