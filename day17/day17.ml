type orientation = Horizontal | Vertical

module IntPair = struct
  type t = int * int * orientation

  let compare = compare
end

module PosAndOrientationToLeastHeatLossMap = Map.Make (IntPair)

type work_item = {
  curr_path_heat_loss : int;
  row : int;
  col : int;
  arriving_orientation : orientation;
}

module WorkItem = struct
  type t = work_item

  let compare = compare
end

module WorkSet = Set.Make (WorkItem)

let rec ints n () = Seq.Cons (n, ints (n + 1))

let parse filename =
  let parse_line map (row_id, line) =
    let row = Array.get map row_id in
    line
    |> String.iteri (fun col_id e ->
           let loss = e |> Char.escaped |> Int32.of_string |> Int32.to_int in
           Array.set row col_id loss);
    map
  in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  let num_rows = List.length lines in
  let num_cols = lines |> List.hd |> String.length in
  let indexed_lines = List.mapi (fun i l -> (i, l)) lines in
  let map =
    List.fold_left parse_line
      (Array.make_matrix num_rows num_cols 0)
      indexed_lines
  in
  map

let calculate_least_heat_lost map start_row start_col num_rows num_cols
    min_steps max_steps =
  let valid_row row = 0 <= row && row < num_rows in
  let valid_col col = 0 <= col && col < num_cols in
  let inc_steps = ints 1 |> Seq.take max_steps |> List.of_seq in
  let dec_steps = inc_steps |> List.map Int.neg in
  let prefix_sum positions =
    let folder acc (r, c) =
      let loss = map.(r).(c) in
      match acc with
      | last_loss :: _ -> (loss + last_loss) :: acc
      | [] -> [ loss ]
    in
    List.fold_left folder [] positions |> List.rev
  in
  let create_visting_work_items step_to_pos_mapper curr_path_heat_loss
      orientation work_set =
    let helper steps work_set =
      let valid_cells = steps |> List.filter_map step_to_pos_mapper in
      let prefixed_heat_losses = prefix_sum valid_cells in
      let trimmer x = List.filteri (fun i _ -> i >= min_steps - 1) x in
      List.combine (trimmer valid_cells) (trimmer prefixed_heat_losses)
      |> List.fold_left
           (fun acc ((r, c), prefixed_heat_loss) ->
             WorkSet.add
               {
                 row = r;
                 col = c;
                 curr_path_heat_loss = curr_path_heat_loss + prefixed_heat_loss;
                 arriving_orientation = orientation;
               }
               acc)
           work_set
    in
    work_set |> helper inc_steps |> helper dec_steps
  in
  let rec explore_path pos_and_orientation_to_least_heat_loss work_set =
    if WorkSet.is_empty work_set then pos_and_orientation_to_least_heat_loss
    else
      let elem = WorkSet.min_elt work_set in
      let { row; col; curr_path_heat_loss; arriving_orientation } = elem in
      let new_work_set = WorkSet.remove elem work_set in
      if
        curr_path_heat_loss
        > PosAndOrientationToLeastHeatLossMap.find
            (row, col, arriving_orientation)
            pos_and_orientation_to_least_heat_loss
      then explore_path pos_and_orientation_to_least_heat_loss new_work_set
      else
        let new_pos_and_orientation_to_least_heat_loss =
          PosAndOrientationToLeastHeatLossMap.update
            (row, col, arriving_orientation)
            (fun _ -> Some curr_path_heat_loss)
            pos_and_orientation_to_least_heat_loss
        in
        let new_work_set =
          match arriving_orientation with
          | Horizontal ->
              create_visting_work_items
                (fun i ->
                  if valid_row (row + i) then Some (row + i, col) else None)
                curr_path_heat_loss Vertical new_work_set
          | Vertical ->
              create_visting_work_items
                (fun i ->
                  if valid_col (col + i) then Some (row, col + i) else None)
                curr_path_heat_loss Horizontal new_work_set
        in
        explore_path new_pos_and_orientation_to_least_heat_loss new_work_set
  in

  let pos_and_orientation_to_path_length =
    ints 0 |> Seq.take num_rows
    |> Seq.flat_map (fun row ->
           ints 0 |> Seq.take num_cols
           |> Seq.flat_map (fun col ->
                  let i = Int.max_int - 10 in
                  Seq.return ((row, col, Horizontal), i)
                  |> Seq.cons ((row, col, Vertical), i)))
    |> PosAndOrientationToLeastHeatLossMap.of_seq
  in
  let start_pos =
    {
      row = start_row;
      col = start_col;
      curr_path_heat_loss = 0;
      arriving_orientation = Horizontal;
    }
  in
  let final_pos_and_orientation_to_path_length =
    WorkSet.(
      empty |> add start_pos
      |> add { start_pos with arriving_orientation = Vertical })
    |> explore_path pos_and_orientation_to_path_length
  in
  let hor =
    PosAndOrientationToLeastHeatLossMap.find
      (num_rows - 1, num_cols - 1, Horizontal)
      final_pos_and_orientation_to_path_length
  in
  let ver =
    PosAndOrientationToLeastHeatLossMap.find
      (num_rows - 1, num_cols - 1, Vertical)
      final_pos_and_orientation_to_path_length
  in
  min hor ver

let () =
  let filename = Sys.argv.(1) in
  let open Printf in
  let map = parse filename in
  let num_rows = Array.length map in
  let num_cols = Array.get map 0 |> Array.length in
  printf "%d\n" (calculate_least_heat_lost map 0 0 num_rows num_cols 1 3);
  printf "%d\n" (calculate_least_heat_lost map 0 0 num_rows num_cols 4 10)
