open State_monad
open Game_defs
open Board_functions
open Ship_functions
open Shoot_functions

let rec play_turn (change : bool) (player : player_opt) (opponent : player_opt) : string StGameState.t =
  let print_endline' s =
    if (not player.is_bot) then print_endline s
  and print_endline_ s pos =
    if player.is_bot then print_endline (s ^ ": " ^ position_string pos)
    else print_endline s
  and print_board' b o s =
    if (not player.is_bot) then print_board b o s in
  if change then
    begin
      print_endline ("\n" ^ str_repeat "*" 70 ^ "\n");
      Printf.printf "Player %s turn\n" (string_of_player player);
      flush stdout; 
    end;
  StGameState.get >>= fun current_state ->
  let player_state   = player_state current_state player 
  and opponent_state = player_state current_state opponent in
  print_endline' "\n   Your board:";
  print_board' player_state.board 0 true;
  print_endline' ((str_repeat " " 33) ^ "Opponent board:");
  print_board' opponent_state.board   30 false;
  let move = if player.is_bot 
    then bot_move opponent_state.board opponent_state.lastHit
    else player_move opponent_state.board in
    match move with
    | Hit, pos  ->
      print_endline_ "Hit and Unsunk" pos;
      let dir = get_dir pos opponent_state.board opponent_state.lastHit in
      let updated_state = 
        { opponent_state with lastHit = Some (pos, dir) } in
      let set_result =
        if opponent.name = A then StGameState.setA updated_state
        else StGameState.setB updated_state in
      set_result >>= fun () ->
      play_turn false player opponent
    | Miss, pos ->
      print_endline_ "Miss" pos;
      play_turn true opponent player
    | Checked, pos ->
      if opponent_state.board.(snd pos).(fst pos).checked = UncheckedShipSurrounding
      then print_endline' "Surrounding of a sunken ship. No more ships around."
      else print_endline' "This coordinate has already been checked.";
      play_turn false player opponent
    | HitAndSunk s, pos ->
      print_endline_ ("Hit and Sunk [" ^ string_of_ship_type s ^ "]") pos;
      let updated_fleet = fleet_after_sunk opponent_state.fleet s in
      let updated_board = 
        board_after_sunk (List.hd (sunk updated_fleet)) opponent_state.board
      in let updated_state = 
        { PlayerState.board = updated_board; fleet = updated_fleet; lastHit = None } in
      let set_result =
        if opponent.name = A then StGameState.setA updated_state
        else StGameState.setB updated_state in
      set_result >>= fun () ->
      if List.length (fst updated_fleet) = 0 then
        begin
          print_endline "\n   Winner's board:";
          print_board player_state.board 0 true;
          print_endline ((str_repeat " " 33) ^ "Loser's board:");
          print_board opponent_state.board 30 true;
          StGameState.return ("Player " ^ (string_of_player player) ^ " wins")
        end
      else
        play_turn false player opponent

let play (is_A_bot : bool) (is_B_bot : bool) : unit =
  let a = {name = A; is_bot = is_A_bot} 
  and b = {name = B; is_bot = is_B_bot} in
  let res = 
    StGameState.run 
      { playerA = PlayerAState.initial_state;
        playerB = PlayerBState.initial_state;
      }
      ( place_ships a >>= fun _ ->
        place_ships b >>= fun _ ->
        play_turn true a b
      )
  in print_endline res 

let () =
  play false true