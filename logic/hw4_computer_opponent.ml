
open! Core
open Hw2_animal_chess_logic

(* Give each animal a value. *)
let animal_value = function
  | Animal.Rat -> 1
  | Cat -> 2
  | Dog -> 3
  | Wolf -> 4
  | Leopard -> 5
  | Tiger -> 6
  | Lion -> 7
  | Elephant -> 8
;;

(* Evaluate the board from Red's perspective. *)
let heuristic_value (state : Game_state.t) =
  match state.decision with
  | Decision.Winner Red -> 1_000_000
  | Winner Blue -> -1_000_000
  | In_progress _ ->
    Map.fold state.board ~init:0 ~f:(fun ~key:_ ~data:piece score ->
      let value = animal_value piece.animal in
      match piece.player with
      | Red -> score + value
      | Blue -> score - value)
;;

(* Random opponent *)
let random_move (state : Game_state.t) : Move.t option =
  match Game_state.get_all_moves state with
  | [] -> None
  | moves ->
    let index = Random.int (List.length moves) in
    Some (List.nth_exn moves index)
;;

(* Generate and sort valid successor states. *)
let children state ~sort_by_whose_turn =
  let compare =
    match sort_by_whose_turn with
    | Player_kind.Red -> Int.descending
    | Blue -> Int.ascending
  in
  Game_state.get_all_moves state
  |> List.filter_map ~f:(fun move ->
    Game_state.make_move state move |> Result.ok)
  |> List.sort
       ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

(* Alpha-beta search *)
let rec alpha_beta_value
    (state : Game_state.t)
    depth
    alpha
    beta
  =
  match state.decision with
  | Decision.In_progress { whose_turn } when depth > 0 ->
    (match whose_turn with
     | Red ->
       List.fold_until
         (children state ~sort_by_whose_turn:whose_turn)
         ~init:(-1_000_001, alpha)
         ~finish:(fun (value, _) -> value)
         ~f:(fun (value, alpha) child ->
           let value =
             Int.max value
               (alpha_beta_value child (depth - 1) alpha beta)
           in
           let alpha = Int.max alpha value in
           if value >= beta
           then Stop value
           else Continue (value, alpha))
     | Blue ->
       List.fold_until
         (children state ~sort_by_whose_turn:whose_turn)
         ~init:(1_000_001, beta)
         ~finish:(fun (value, _) -> value)
         ~f:(fun (value, beta) child ->
           let value =
             Int.min value
               (alpha_beta_value child (depth - 1) alpha beta)
           in
           let beta = Int.min beta value in
           if value <= alpha
           then Stop value
           else Continue (value, beta)))
  | _ -> heuristic_value state
;;

(* Better opponent *)
let better_move (state : Game_state.t) ~depth : Move.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | In_progress { whose_turn } ->
    let moves =
      Game_state.get_all_moves state
      |> List.filter_map ~f:(fun move ->
        Game_state.make_move state move
        |> Result.ok
        |> Option.map ~f:(fun child ->
          let value =
            alpha_beta_value
              child
              (depth - 1)
              (-1_000_001)
              1_000_001
          in
          move, value))
    in
    let best =
      match whose_turn with
      | Red ->
        List.max_elt moves
          ~compare:(fun (_, a) (_, b) -> Int.compare a b)
      | Blue ->
        List.min_elt moves
          ~compare:(fun (_, a) (_, b) -> Int.compare a b)
    in
    Option.map best ~f:fst
;;

let rec play_game (state : Game_state.t) =
  match state.decision with
  | Decision.Winner player -> Some player
  | In_progress { whose_turn } ->
    let move =
      match whose_turn with
      | Red -> better_move state ~depth:2
      | Blue -> random_move state
    in
    match move with
    | None -> None
    | Some move ->
      (match Game_state.make_move state move with
       | Ok next_state -> play_game next_state
       | Error _ -> None)
;;
