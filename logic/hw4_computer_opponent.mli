open! Core
open Hw2_animal_chess_logic

val heuristic_value : Game_state.t -> int

val random_move : Game_state.t -> Move.t option

val better_move :
  Game_state.t -> depth:int -> Move.t option

val play_game :
  Game_state.t -> Player_kind.t option
