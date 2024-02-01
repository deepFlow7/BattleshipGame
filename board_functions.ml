open Game_defs

(* czy można umieścić statek na danej pozycji *)
let correct_pos : position -> direction -> ship_type -> board_t -> bool =
  fun pos dir ship_type board ->
    let size = (get_ship_size ship_type) 
    and (x, y) = pos in 
    match dir with
    | Horizontal -> 
      x + size - 1 < 10 && 
      List.for_all 
        (fun idx -> board.(y).(idx).ship_status = Empty) 
        (List.init (get_ship_size ship_type) (fun a -> a + x))
    | Vertical   -> 
      y + size - 1 < 10 && 
      List.for_all 
        (fun idx -> board.(idx).(x).ship_status = Empty) 
        (List.init (get_ship_size ship_type) (fun a -> a + y))

(* zmodyfikuj pole *)
let mark_cell : int -> int -> board_t -> cell_status -> ship_status -> unit =
  fun x y board checked ship_status ->
    if 0 <= x && x <= 9 && 0 <= y && y <= 9 &&  board.(y).(x).checked != Checked then
      board.(y).(x) <- { checked = checked; ship_status = ship_status }

(* zaznacz statek na planszy *)
let rec mark_ship : ship -> cell_status -> board_t -> board_t =
  fun ship checked board -> 
    let mark' x y ship_status =
      if checked != UncheckedShipSurrounding || ship_status = ShipSurrounding
      then mark_cell x y board checked ship_status 
    in
    let (ship_type, (x, y), dir) =  ship in
    let n = get_ship_size ship_type in
    let rec mark_cells x y dir board n =
      begin match dir with
      | Horizontal -> 
        mark' x (y - 1) ShipSurrounding;
        mark' x    y    (Ship ship_type);
        mark' x (y + 1) ShipSurrounding
      | Vertical   -> 
        mark' (x - 1) y ShipSurrounding;
        mark'    x    y (Ship ship_type);
        mark' (x + 1) y ShipSurrounding
      end;
      if n > 1 then
        match dir with
        | Horizontal -> mark_cells (x + 1) y dir board (n - 1)
        | Vertical   -> mark_cells x (y + 1) dir board (n - 1)
    in
    begin match dir with
    | Horizontal -> 
      mark_cells (x - 1) y dir board (n + 2);
      mark' (x - 1) y ShipSurrounding;
      mark' (x + n) y ShipSurrounding;
    | Vertical   -> 
      mark_cells x (y - 1) dir board (n + 2);
      mark' x (y - 1) ShipSurrounding;
      mark' x (y + n) ShipSurrounding;
    end;
    board

(* zaznacz pola wokół zatopionego statku jako niedostępne *)
let board_after_sunk : ship -> board_t -> board_t =
  fun ship board ->
    mark_ship ship UncheckedShipSurrounding board

let print_board : board_t -> int -> bool -> unit =
  fun b offset show_unchecked ->
    let offset_str = str_repeat " " offset in 
    print_endline (offset_str ^ "   " ^ (str_repeat "-" 21));
    print_endline (offset_str ^ "  | 0 1 2 3 4 5 6 7 8 9 |");
    print_endline (offset_str ^ "   " ^ (str_repeat "-" 21));
    Array.iteri (fun i row ->
      print_string (offset_str ^ (string_of_int i) ^ " | ");
      Array.iter (fun cell ->
        let cell_str =
          match show_unchecked, cell.checked, cell.ship_status with
          | false, Unchecked, _  
          | true, Unchecked, Empty           -> "· "
          | true, Unchecked, Ship _          -> "O "
          | _, Checked, Ship _               -> "S "
          | _, Checked, _                    -> "x "
          | true, Unchecked, ShipSurrounding
          | _, UncheckedShipSurrounding, _   -> "- "
        in
        print_string cell_str
      ) row;
      print_string "|\n"
    ) b;
    print_endline (offset_str ^ "   " ^ (str_repeat "-" 21))