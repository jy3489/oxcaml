open! Core

module Player_kind = struct
  type t =
    | Red
    | Blue
  [@@deriving sexp, compare, equal]

  let opposite t =
    match t with
    | Red -> Blue
    | Blue -> Red
  ;;
end

module Animal = struct
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

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Piece = struct
  type t =
    { player : Player_kind.t
    ; animal : Animal.t
    }
  [@@deriving sexp, compare, equal]
end

module Move = struct
  type t =
    { from_pos : Cell_position.t
    ; to_pos : Cell_position.t
    }
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Winner _ -> true
    | In_progress _ -> false
  ;;
end
module Game_state = struct
  open Animal
  open Player_kind
  open Decision

  type t =
    { board : Piece.t Cell_position.Map.t
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, compare, equal]

    let initial_board : Piece.t Cell_position.Map.t =
    Cell_position.Map.of_alist_exn
      [
        { row = 0; column = 0 }, { Piece.player = Red; animal = Lion };
        { row = 0; column = 6 }, { Piece.player = Red; animal = Tiger };
        { row = 1; column = 1 }, { Piece.player = Red; animal = Dog };
        { row = 1; column = 5 }, { Piece.player = Red; animal = Cat };
        { row = 2; column = 0 }, { Piece.player = Red; animal = Rat };
        { row = 2; column = 2 }, { Piece.player = Red; animal = Leopard };
        { row = 2; column = 4 }, { Piece.player = Red; animal = Wolf };
        { row = 2; column = 6 }, { Piece.player = Red; animal = Elephant };

        { row = 6; column = 0 }, { Piece.player = Blue; animal = Elephant };
        { row = 6; column = 2 }, { Piece.player = Blue; animal = Wolf };
        { row = 6; column = 4 }, { Piece.player = Blue; animal = Leopard };
        { row = 6; column = 6 }, { Piece.player = Blue; animal = Rat };
        { row = 7; column = 1 }, { Piece.player = Blue; animal = Cat };
        { row = 7; column = 5 }, { Piece.player = Blue; animal = Dog };
        { row = 8; column = 0 }, { Piece.player = Blue; animal = Tiger };
        { row = 8; column = 6 }, { Piece.player = Blue; animal = Lion };
      ]
  ;;

  let create () =
    { board = initial_board
    ; decision = In_progress { whose_turn = Red }
    ; last_move = None
    }
  ;;

  let is_legal_cell_position ({ row; column } : Cell_position.t) =
    row >= 0 && row < 9 && column >= 0 && column < 7
  ;;
    module Move_error = struct
    type t =
      | Game_is_over
      | Illegal_cell_position
      | No_piece_at_position
      | Not_your_piece
      | Illegal_move
      | Cannot_capture
    [@@deriving sexp, compare]
  end

    let animal_rank animal =
    match animal with
    | Rat -> 1
    | Cat -> 2
    | Dog -> 3
    | Wolf -> 4
    | Leopard -> 5
    | Tiger -> 6
    | Lion -> 7
    | Elephant -> 8
  ;;

  let is_river ({ row; column } : Cell_position.t) =
    row >= 3
    && row <= 5
    && (column = 1 || column = 2 || column = 4 || column = 5)
  ;;

  let is_red_den ({ row; column } : Cell_position.t) =
    row = 0 && column = 3
  ;;

  let is_blue_den ({ row; column } : Cell_position.t) =
    row = 8 && column = 3
  ;;

  let is_own_den player position =
    match player with
    | Red -> is_red_den position
    | Blue -> is_blue_den position
  ;;

  let is_opponent_den player position =
    match player with
    | Red -> is_blue_den position
    | Blue -> is_red_den position
  ;;

  let is_adjacent
      ({ row = row1; column = column1 } : Cell_position.t)
      ({ row = row2; column = column2 } : Cell_position.t)
    =
    let row_diff = Int.abs (row1 - row2) in
    let column_diff = Int.abs (column1 - column2) in
    (row_diff = 1 && column_diff = 0)
    || (row_diff = 0 && column_diff = 1)
  ;;

  let can_enter_river animal =
    match animal with
    | Rat -> true
    | _ -> false
  ;;

  let can_capture (attacker : Piece.t) (defender : Piece.t) =
    match attacker.animal, defender.animal with
    | Rat, Elephant -> true
    | Elephant, Rat -> false
    | _ -> animal_rank attacker.animal >= animal_rank defender.animal
  ;;

  let is_basic_move_legal
      (piece : Piece.t)
      (from_pos : Cell_position.t)
      (to_pos : Cell_position.t)
    =
    is_adjacent from_pos to_pos
    && not (is_own_den piece.player to_pos)
    && (not (is_river to_pos) || can_enter_river piece.animal)
  ;;

  let make_move t (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ -> Error Game_is_over
    | In_progress { whose_turn } ->
      if
        not (is_legal_cell_position move.from_pos)
        || not (is_legal_cell_position move.to_pos)
      then Error Illegal_cell_position
      else
        match Map.find t.board move.from_pos with
        | None -> Error No_piece_at_position
        | Some piece ->
          if not (Player_kind.equal piece.player whose_turn)
          then Error Not_your_piece
          else if not (is_basic_move_legal piece move.from_pos move.to_pos)
          then Error Illegal_move
          else
            match Map.find t.board move.to_pos with
            | Some target when Player_kind.equal target.player piece.player ->
              Error Illegal_move
            | Some target when not (can_capture piece target) ->
              Error Cannot_capture
            | _ ->
  let board =
    let board = Map.remove t.board move.from_pos in
    Map.set board ~key:move.to_pos ~data:piece
  in
  let decision =
    if is_opponent_den piece.player move.to_pos
    then Winner piece.player
    else In_progress { whose_turn = Player_kind.opposite whose_turn }
  in
  Ok { board; decision; last_move = Some move }
;;
end