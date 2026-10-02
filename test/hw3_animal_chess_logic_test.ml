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

let%test "Illegal move: destination is outside the board" =
  let move : Move.t =
    { from_pos = { row = 0; column = 0 }
    ; to_pos = { row = -1; column = 0 }
    }
  in

  match Game_state.make_move initial_state move with
  | Error Game_state.Move_error.Illegal_cell_position -> true
  | _ -> false
;;

let%test "Illegal move: no piece at starting position" =
  let move : Move.t =
    { from_pos = { row = 4; column = 3 }
    ; to_pos = { row = 4; column = 4 }
    }
  in

  match Game_state.make_move initial_state move with
  | Error Game_state.Move_error.No_piece_at_position -> true
  | _ -> false
;;

let%test "Illegal move: trying to move opponent's piece" =
  let move : Move.t =
    { from_pos = { row = 6; column = 0 }
    ; to_pos = { row = 5; column = 0 }
    }
  in

  match Game_state.make_move initial_state move with
  | Error Game_state.Move_error.Not_your_piece -> true
  | _ -> false
;;

let%test "Illegal move: non-Rat piece cannot enter river" =
  let board =
    Cell_position.Map.of_alist_exn
      [ { row = 3; column = 0 },
        { Piece.player = Player_kind.Red; animal = Animal.Cat }
      ]
  in

  let state : Game_state.t =
    { board
    ; decision = Decision.In_progress { whose_turn = Player_kind.Red }
    ; last_move = None
    }
  in

  let move : Move.t =
    { from_pos = { row = 3; column = 0 }
    ; to_pos = { row = 3; column = 1 }
    }
  in

  match Game_state.make_move state move with
  | Error Game_state.Move_error.Illegal_move -> true
  | _ -> false
;;

let%test "Legal move: Rat can enter river" =
  let board =
    Cell_position.Map.of_alist_exn
      [ { row = 3; column = 0 },
        { Piece.player = Player_kind.Red; animal = Animal.Rat }
      ]
  in

  let state : Game_state.t =
    { board
    ; decision = Decision.In_progress { whose_turn = Player_kind.Red }
    ; last_move = None
    }
  in

  let move : Move.t =
    { from_pos = { row = 3; column = 0 }
    ; to_pos = { row = 3; column = 1 }
    }
  in

  match Game_state.make_move state move with
  | Ok new_state ->
    (match Map.find new_state.board { row = 3; column = 1 } with
     | Some piece ->
       Piece.equal
         piece
         { player = Player_kind.Red
         ; animal = Animal.Rat
         }
     | None -> false)
  | Error _ -> false
;;

let%test "Illegal capture: Cat cannot capture Dog" =
  let board =
    Cell_position.Map.of_alist_exn
      [ { row = 2; column = 3 },
        { Piece.player = Player_kind.Red; animal = Animal.Cat }
      ; { row = 2; column = 4 },
        { Piece.player = Player_kind.Blue; animal = Animal.Dog }
      ]
  in

  let state : Game_state.t =
    { board
    ; decision = Decision.In_progress { whose_turn = Player_kind.Red }
    ; last_move = None
    }
  in

  let move : Move.t =
    { from_pos = { row = 2; column = 3 }
    ; to_pos = { row = 2; column = 4 }
    }
  in

  match Game_state.make_move state move with
  | Error Game_state.Move_error.Cannot_capture -> true
  | _ -> false
;;

let random_walk (initial_state : Game_state.t) ~random_seed ~max_steps =
  Core.Random.init random_seed;

  let rec walk (state : Game_state.t) steps =
    if steps >= max_steps || Decision.is_game_over state.decision
    then state
    else
      let legal_next_states =
        Game_state.get_all_moves state
        |> List.filter_map ~f:(fun move ->
          Game_state.make_move state move |> Result.ok)
      in

      match List.random_element legal_next_states with
      | None -> state
      | Some next_state -> walk next_state (steps + 1)
  in

  walk initial_state 0
;;

let%test "Random exploration produces valid game states" =
  let final_state =
    random_walk initial_state ~random_seed:1 ~max_steps:100
  in
  Map.for_alli final_state.board ~f:(fun ~key ~data:_ ->
    key.row >= 0
    && key.row < 9
    && key.column >= 0
    && key.column < 7)
;;