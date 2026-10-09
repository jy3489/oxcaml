
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
  let valid_moves =
    Game_state.get_all_moves state
    |> List.filter ~f:(fun move ->
      match Game_state.make_move state move with
      | Ok _ -> true
      | Error _ -> false)
  in
  match valid_moves with
  | [] -> None
  | moves ->
    Some (List.nth_exn moves (Random.int (List.length moves)))
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


(* Alpha-beta search with a deadline. *)
exception Search_timeout

let check_time deadline =
  if Time_ns.( > ) (Time_ns.now ()) deadline
  then raise Search_timeout
;;

let rec alpha_beta_value
    (state : Game_state.t)
    depth
    alpha
    beta
    deadline
  =
  check_time deadline;
  match state.decision with
  | Decision.In_progress { whose_turn } when depth > 0 ->
    let next_states =
      children state ~sort_by_whose_turn:whose_turn
    in
    (match whose_turn with
     | Red ->
       List.fold_until next_states
         ~init:(-1_000_001, alpha)
         ~finish:(fun (value, _) -> value)
         ~f:(fun (value, alpha) child ->
           check_time deadline;
           let value =
             Int.max value
               (alpha_beta_value child (depth - 1) alpha beta deadline)
           in
           let alpha = Int.max alpha value in
           if value >= beta
           then Stop value
           else Continue (value, alpha))
     | Blue ->
       List.fold_until next_states
         ~init:(1_000_001, beta)
         ~finish:(fun (value, _) -> value)
         ~f:(fun (value, beta) child ->
           check_time deadline;
           let value =
             Int.min value
               (alpha_beta_value child (depth - 1) alpha beta deadline)
           in
           let beta = Int.min beta value in
           if value <= alpha
           then Stop value
           else Continue (value, beta)))
  | _ -> heuristic_value state
;;

(* Better opponent with iterative deepening. *)
let better_move (state : Game_state.t) ~depth : Move.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | In_progress { whose_turn } ->
    let deadline =
      Time_ns.add (Time_ns.now ())
        (Time_ns.Span.of_sec 1.8)
    in
    let valid_moves =
      Game_state.get_all_moves state
      |> List.filter ~f:(fun move ->
        Result.is_ok (Game_state.make_move state move))
    in
    (match valid_moves with
     | [] -> None
     | first_move :: _ ->
       let best_move = ref first_move in
       let rec search current_depth =
         if current_depth > depth
         then ()
         else (
           try
             check_time deadline;
             let scored_moves =
               List.map valid_moves ~f:(fun move ->
                 check_time deadline;
                 let child =
                    match Game_state.make_move state move with
                    | Ok child -> child
                    | Error _ -> failwith "Invalid move"
                 in
                 let score =
                   alpha_beta_value
                     child (current_depth - 1)
                     (-1_000_001) 1_000_001 deadline
                 in
                 move, score)
             in
             let best =
               match whose_turn with
               | Red ->
                 List.max_elt scored_moves
                   ~compare:(fun (_, a) (_, b) -> Int.compare a b)
               | Blue ->
                 List.min_elt scored_moves
                   ~compare:(fun (_, a) (_, b) -> Int.compare a b)
             in
             Option.iter best ~f:(fun (move, _) ->
               best_move := move);
             search (current_depth + 1)
           with Search_timeout -> ())
       in
       search 1;
       Some !best_move)
;;


(* Play one game with a maximum move limit. *)
let play_game ~better_player ~max_moves =
  let rec loop (state : Game_state.t) moves_left =
    match state.decision with
    | Decision.Winner player -> Some player
    | In_progress _ when moves_left <= 0 -> None
    | In_progress { whose_turn } ->
      let move =
        if Player_kind.equal whose_turn better_player
        then better_move state ~depth:2
        else random_move state
      in
      match move with
      | None -> None
      | Some move ->
        (match Game_state.make_move state move with
         | Ok next_state -> loop next_state (moves_left - 1)
         | Error _ -> None)
  in
  loop (Game_state.create ()) max_moves
;;


(* Run games and count the results. *)
let run_games ~num_games ~max_moves =
  let rec loop n better_wins random_wins draws =
    if n >= num_games
    then better_wins, random_wins, draws
    else (
      let better_player =
        if n mod 2 = 0 then Player_kind.Red else Player_kind.Blue
      in
      let winner = play_game ~better_player ~max_moves in
      match winner with
      | None ->
        loop (n + 1) better_wins random_wins (draws + 1)
      | Some player ->
        if Player_kind.equal player better_player
        then loop (n + 1) (better_wins + 1) random_wins draws
        else loop (n + 1) better_wins (random_wins + 1) draws)
  in
  loop 0 0 0 0
;;

