open Raylib

module Projectile = struct
  type t =
    { x: float
    ; y: float
    ; dx: float
    ; dy: float
    ; lifetime: float
    }
end

type t =
  { x: float
  ; y: float
  ; dx: float
  ; dy: float
  ; mx: float
  ; projs: Projectile.t list
  }

type plug_t =
  { mutable fresh: int -> int -> t
  ; mutable update: float -> t -> t
  }

exception Plugin_not_loaded

let plug =
  { fresh = (fun _ _ -> raise Plugin_not_loaded)
  ; update = (fun _ _ -> raise Plugin_not_loaded)
  }
