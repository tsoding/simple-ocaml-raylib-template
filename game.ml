open Raylib

module Projectile = struct
  type t =
    { pos: Vector2.t
    ; vel: Vector2.t
    ; lifetime: float
    }
end

module Explosion = struct
  type t =
    { pos: Vector2.t
    ; progress: float
    }
end

type t =
  { pos: Vector2.t
  ; vel: Vector2.t
  ; mov: Vector2.t
  ; projs: Projectile.t list
  ; explosions: Explosion.t list
  ; health: float
  }

type plug_t =
  { mutable fresh: unit -> t
  ; mutable update: float -> t -> t
  }

exception Plugin_not_loaded

let plug =
  { fresh = (fun _ -> raise Plugin_not_loaded)
  ; update = (fun _ _ -> raise Plugin_not_loaded)
  }
