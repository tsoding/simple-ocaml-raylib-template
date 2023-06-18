open Raylib

type t =
  { x: float
  ; y: float
  ; dx: float
  ; dy: float
  }

type plug_t =
  { mutable fresh: unit -> t
  ; mutable update: t -> float -> t
  ; mutable render: t -> unit
  }

exception Plugin_not_loaded

let plug =
  { fresh = (fun _ -> raise Plugin_not_loaded)
  ; update = (fun _ _ -> raise Plugin_not_loaded)
  ; render = (fun _ -> raise Plugin_not_loaded)
  }
