open! Core
open Tictactoe_logic_library
open Hw2_animal_chess_logic
open Hw4_computer_opponent

(* Check that the random opponent picks a legal move. *)
let%test_unit "random opponent chooses a legal move" =
  let state = Game_state.create () in
  let legal_moves = Game_state.get_all_moves state in
  let move = random_move state in
  match move with
  | None -> failwith "Random opponent returned no move"
  | Some move ->
    assert (List.mem legal_moves move ~equal:Move.equal)
;;

(* Check that the better opponent picks a legal move. *)
let%test_unit "better opponent chooses a legal move" =
  let state = Game_state.create () in
  let legal_moves = Game_state.get_all_moves state in
  let move = better_move state ~depth:2 in
  match move with
  | None -> failwith "Better opponent returned no move"
  | Some move ->
    assert (List.mem legal_moves move ~equal:Move.equal)
;;

(* Check that the better opponent returns no move
   when the game has already ended. *)
let%test_unit "better opponent stops after game over" =
  let state = Game_state.create () in
  let finished_state =
    { state with decision = Decision.Winner Player_kind.Red }
  in
  assert (Option.is_none (better_move finished_state ~depth:2))
;;
