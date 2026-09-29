open! Core

module Player_kind : sig
  type t =
    | Red
    | Blue
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Animal : sig
  type t =
    | Rat
    | Cat
    | Dog
    | Wolf
    | Leopard
    | Tiger
    | Lion
    | Elephant
  [@@deriving sexp, compare, equal]
end

module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare]

  include Comparable.S with type t := t
end

module Piece : sig
  type t =
    { player : Player_kind.t
    ; animal : Animal.t
    }
  [@@deriving sexp, compare, equal]
end

module Move : sig
  type t =
    { from_pos : Cell_position.t
    ; to_pos : Cell_position.t
    }
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  type t =
    { board : Piece.t Cell_position.Map.t
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

  val create : unit -> t

  module Move_error : sig
    type t =
      | Game_is_over
      | Illegal_cell_position
      | No_piece_at_position
      | Not_your_piece
      | Illegal_move
      | Cannot_capture
    [@@deriving sexp, compare]
  end

  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end