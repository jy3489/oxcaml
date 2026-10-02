open! Core
open Tictactoe_logic_library
open Hw2_animal_chess_logic

let ok_exn result =
  Result.ok result |> Option.value_exn
;;

let initial_state =
  Game_state.create ()
;;

let%test "HW1 initial move: Red Lion moves from (0,0) to (1,0)" =
  let move : Move.t =
    { from_pos = { row = 0; column = 0 }
    ; to_pos = { row = 1; column = 0 }
    }
  in

  match Game_state.make_move initial_state move with
  | Error _ -> false
  | Ok new_state ->
    match Map.find new_state.board { row = 1; column = 0 } with
    | None -> false
    | Some piece ->
      Piece.equal
        piece
        { player = Player_kind.Red
        ; animal = Animal.Lion
        }
;;

let%test "HW1 interesting state: Blue Rat captures Red Elephant" =
  let board =
    Cell_position.Map.of_alist_exn
      [ { row = 2; column = 3 },
        { Piece.player = Player_kind.Red; animal = Animal.Elephant }
      ; { row = 2; column = 4 },
        { Piece.player = Player_kind.Blue; animal = Animal.Rat }
      ]
  in

  let state : Game_state.t =
    { board
    ; decision = Decision.In_progress { whose_turn = Player_kind.Blue }
    ; last_move = None
    }
  in

  let move : Move.t =
    { from_pos = { row = 2; column = 4 }
    ; to_pos = { row = 2; column = 3 }
    }
  in

  match Game_state.make_move state move with
  | Error _ -> false
  | Ok new_state ->
    match Map.find new_state.board { row = 2; column = 3 } with
    | None -> false
    | Some piece ->
      Piece.equal
        piece
        { player = Player_kind.Blue
        ; animal = Animal.Rat
        }
;;

let%test "HW1 winning state: Red Tiger enters Blue den" =
  let board =
    Cell_position.Map.of_alist_exn
      [ { row = 7; column = 3 },
        { Piece.player = Player_kind.Red; animal = Animal.Tiger }
      ]
  in

  let state : Game_state.t =
    { board
    ; decision = Decision.In_progress { whose_turn = Player_kind.Red }
    ; last_move = None
    }
  in

  let move : Move.t =
    { from_pos = { row = 7; column = 3 }
    ; to_pos = { row = 8; column = 3 }
    }
  in

  match Game_state.make_move state move with
  | Error _ -> false
  | Ok new_state ->
    Decision.equal
      new_state.decision
      (Decision.Winner Player_kind.Red)
;;
