open! Core
open Hw2_animal_chess_logic

val heuristic_value : Game_state.t -> int

val random_move : Game_state.t -> Move.t option

val better_move :
  Game_state.t -> depth:int -> Move.t option

val play_game :
  better_player:Player_kind.t ->
  max_moves:int ->
  Player_kind.t option

val run_games :
  num_games:int ->
  max_moves:int ->
  int * int * int
