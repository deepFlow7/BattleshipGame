(* Ships and board ---------------------------- *)
type ship_type =
  | Carrier
  | Battleship
  | Destroyer
  | Submarine
  | PatrolBoat

type position = int * int
type direction = 
  | Horizontal
  | Vertical

type ship = ship_type * position * direction 

type cell_status = 
  | Checked
  | Unchecked
  | UncheckedShipSurrounding (* wokół zatopionego statku nie może być innego *)

type cell = { checked: cell_status; ship_status: ship_status }
and ship_status =
  | Empty
  | Ship of ship_type
  | ShipSurrounding

type board_t = cell array array

(* Shots -------------------------------------- *)
type shot = 
  | Hit 
  | Miss
  | HitAndSunk of ship_type
  | Checked

type move_direction = (* do zapamiętania strzału bota *)
  | Up
  | Down
  | Left
  | Right
  | Unknown

(* Game state  -------------------------------- *)
module PlayerState = struct
  type t = {
    board   : board_t;
    fleet   : (ship list * ship list); (* [unsunk], [sunk] *)
    lastHit : (position * move_direction) option; 
  }
  let create_board () =
    Array.make_matrix 10 10 { checked = Unchecked; ship_status = Empty }
end

module PlayerAState = struct
  include PlayerState
  let initial_state = {
    board = create_board ();
    fleet = ([], []);
    lastHit = None;
  }
end

module PlayerBState = struct
  include PlayerState
  let initial_state = {
    board = create_board ();
    fleet = ([], []);
    lastHit = None;
  }
end

module GameState = struct
  type t = {
    playerA : PlayerState.t;
    playerB : PlayerState.t;
  }
end

(* Players and score -------------------------- *)
type player_name = 
  | A
  | B

type player_opt = {
  name   : player_name;
  is_bot : bool;
}

type score =
  | AWins
  | BWins

(* SIMPLE FUNCTIONS --------------------------- *)

(* player functions --------------------------- *)
let player_state (state : GameState.t) (player : player_opt) : PlayerState.t = 
  match player.name with
  | A -> state.GameState.playerA
  | B -> state.GameState.playerB

let string_of_player player =
  match player.name with
  | A -> "A"
  | B -> "B"

(* ship and fleet functions ------------------- *)
let string_of_ship_type ship_type =
  match ship_type with
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Destroyer -> "Destroyer"
  | Submarine -> "Submarine"
  | PatrolBoat -> "Patrol Boat"
  
let get_ship_size = function
  | Carrier -> 5
  | Battleship -> 4
  | Destroyer -> 3
  | Submarine -> 3
  | PatrolBoat -> 2

let sunk fleet =
  snd fleet

let unsunk fleet =
  fst fleet

let fleet_after_sunk fleet ship_type =
  let rec find xs ship_t =
    match xs with
    | [] -> failwith "sunk ship not found"
    | (s_type, _, _) as ship :: xs -> 
      if s_type = ship_type then
        (xs, ship)
      else
        let (rest, sunk_ship) = find xs ship_type in
        (ship :: rest, sunk_ship)
  in let (unsunk, sunk_ship) = find (unsunk fleet) ship_type 
    in (unsunk, sunk_ship :: (sunk fleet)) 

(* move direction functions ------------------- *)
let direction_fun (dir : move_direction) : (position -> position) =
  fun (x, y) ->
    match dir with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
    | Unknown -> (x, y)

let opposite_dir (dir : move_direction) : move_direction =
  match dir with
  | Left -> Right
  | Right -> Left
  | Up -> Down
  | Down -> Up
  | Unknown -> Unknown

(* random functions --------------------------- *)
let () = Random.self_init ()

let generate_random_position () =
  (Random.int 10, Random.int 10)

let generate_random_direction () =
  if Random.bool () then Horizontal else Vertical

(* other functions ---------------------------- *)
let is_in_range (x, y) = 
  x >= 0 && x <= 9 && y >= 0 && y <= 9

let rec str_repeat s n =
  if n = 0 then "" else s ^ str_repeat s (n - 1)

let position_string (x, y) =
  "(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")"


let rec clear_stdin () =
  let _ = read_line () in ()

  let rec get_pos : string -> position =
    fun info -> try
      print_string info;
      flush stdout;
      let input_line = read_line () in
      match Scanf.sscanf input_line "(%d, %d)" (fun x y -> Some (x, y)) with
      | Some pos ->
        if is_in_range pos then pos
        else begin  
          print_endline "Invalid position (x and y must be between 0 and 9)";
          get_pos info
        end
      | _ ->
        print_endline "Incorrect format";
        get_pos info
    with
     _ ->
      print_endline "Incorrect format";
      get_pos info 