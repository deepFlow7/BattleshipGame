open State_monad
open Game_defs
open Board_functions

let is_sunk (x : int) (y : int) (board : cell array array) =
  let rec go f (x, y) board =
    if 0 > x || 0 > y || x > 9 || y > 9 
      then true
    else
      match board.(y).(x) with
      | { checked = Checked; ship_status = Ship _ } -> go f (f x y) board
      | { checked = _; ship_status = ShipSurrounding } -> true
      | _ -> false
  in List.for_all (fun f -> go f (f x y) board) 
  [(fun x y -> (x + 1, y)); (fun x y -> (x - 1, y)); (fun x y -> (x, y + 1)); (fun x y -> (x, y - 1))]

let shoot x y board =
  let cell = board.(y).(x) in
  ((match cell.checked with
    | UncheckedShipSurrounding 
    | Checked -> Checked
    | Unchecked -> 
      mark_cell x y board Checked cell.ship_status;
      match cell.ship_status with
      | Ship s -> 
        if is_sunk x y board 
        then HitAndSunk s
        else Hit
      | _    -> Miss),
    (x, y)
  )

let rec player_move board = 
  let (x, y) = get_pos "Enter the coordinates of the shot in the format: (x,y): "
  in shoot x y board

let random_from_list xs board =
  let xs = List.filter (fun (x,y) -> 
    x >= 0 && x <= 9 && y >= 0 && y <= 9 && board.(y).(x).checked = Unchecked)
    xs in
    let idx = Random.int (List.length xs) in
    List.nth xs idx

let rec bot_move board lastHit =
  let rec go f pos =
    if board.(snd pos).(fst pos).checked = Unchecked
      then pos
    else go f (f pos) in
  let (x, y) = 
  match lastHit with
  | None -> generate_random_position ()
  | Some ((x, y), dir) ->
    match dir with
    | Unknown -> 
      random_from_list [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] board
    | _ ->
      let next_pos = (direction_fun dir (x, y)) in 
      if is_in_range next_pos && board.(snd next_pos).(fst next_pos).checked = Unchecked
        then next_pos 
      else go (direction_fun (opposite_dir dir)) (x, y)  
  in shoot x y board

let get_dir pos board lastHit = 
  match lastHit with
  | None -> Unknown
  | Some ((x, y), dir) -> 
    match dir with
    | Unknown -> 
      List.hd (List.filter (fun d -> direction_fun d (x,y) = pos) [Up; Down; Left; Right])
    | _ ->
      if direction_fun dir (x,y) = pos then dir
      else opposite_dir dir