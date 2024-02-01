open State_monad
open Game_defs
open Board_functions

let place_ship : player_opt -> ship_type -> position -> direction -> unit StGameState.t =
  fun player ship_type pos dir ->
    StGameState.get >>= fun current_state ->
      let player_state = player_state current_state player in
    let ship = (ship_type, pos, dir) in
    let new_board = mark_ship ship Unchecked player_state.board
    and new_unsunk_fleet = ship :: fst player_state.fleet in
    let new_player_board = { PlayerState.board = new_board; fleet = (new_unsunk_fleet, snd player_state.fleet); lastHit = None} in
    if player.name = A 
      then StGameState.setA new_player_board 
    else 
      StGameState.setB new_player_board
   
let rec place_ship_by_player : player_opt -> ship_type -> board_t -> unit StGameState.t =
  fun player ship_type board ->
    let rec get_dir () = 
      try
      Printf.printf "Enter the direction of the ship (H - horizontal or V - Vertical): ";
      flush stdout; 
      let dir_char = Scanf.scanf "%c\n" (fun x -> x) in
      match dir_char with
      | 'V' | 'v' -> Vertical
      | 'H' | 'h' -> Horizontal
      | _ -> 
        print_endline "Invalid direction";
        get_dir ()
      with 
      | _ -> print_endline "Incorrect format"; get_dir ()
    in 
    Printf.printf "Place %s (size %d)\n" (string_of_ship_type ship_type) (get_ship_size ship_type);
    flush stdout; 
    let pos = get_pos "Enter the top-left index of the ship in format: (x,y): "
    and dir = get_dir () in
    if correct_pos pos dir ship_type board then 
      place_ship player ship_type pos dir
    else 
      begin
        print_endline "Invalid position for this ship, try again.";
        place_ship_by_player player ship_type board;
      end

let place_ships_by_player : player_opt -> GameState.t StGameState.t =
  fun player ->
    let place_ship' ship_type = StGameState.get >>= fun current_state ->
      let player_state = player_state current_state player
      in place_ship_by_player player ship_type player_state.board >>= fun () ->
      Printf.printf "Current board:\n";
      print_board player_state.board 0 true;
      StGameState.return () in
      place_ship' Carrier    >>= fun () ->
      place_ship' Battleship >>= fun () ->
      place_ship' Destroyer  >>= fun () ->
      place_ship' Submarine  >>= fun () ->
      place_ship' PatrolBoat >>= fun () -> 
      StGameState.get

let rec place_ship_randomly : player_opt -> ship_type -> board_t -> unit StGameState.t =
  fun player ship_type board -> 
    let pos = generate_random_position ()
    and dir = generate_random_direction () in
    if correct_pos pos dir ship_type board then
      place_ship player ship_type pos dir
    else place_ship_randomly player ship_type board

let place_ships_randomly : player_opt -> GameState.t StGameState.t =
  fun player ->
    let place_ship' ship_type = StGameState.get >>= fun current_state ->
      let player_state = player_state current_state player
      in place_ship_randomly player ship_type player_state.board in
        place_ship' Carrier    >>= fun () ->
        place_ship' Battleship >>= fun () ->
        place_ship' Destroyer  >>= fun () ->
        place_ship' Submarine  >>= fun () ->
        place_ship' PatrolBoat >>= fun () -> 
        StGameState.get

let place_ships : player_opt -> GameState.t StGameState.t =
  fun player ->
    if player.is_bot 
      then place_ships_randomly player
    else place_ships_by_player player