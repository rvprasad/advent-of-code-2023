type corner = { x : int; y : int; z : int }
type brick = { first_corner : corner; second_corner : corner }

let brick_comparator a b =
  let tmpa =
    [
      a.first_corner.z;
      a.second_corner.z;
      a.first_corner.y;
      a.second_corner.y;
      a.first_corner.x;
      a.second_corner.x;
    ]
  in
  let tmpb =
    [
      b.first_corner.z;
      b.second_corner.z;
      b.first_corner.y;
      b.second_corner.y;
      b.first_corner.x;
      b.second_corner.x;
    ]
  in
  if tmpa < tmpb then -1 else if tmpa = tmpb then 0 else 1

(* let pp_brick out (b : brick) =
   Format.fprintf out "((x1=%d; y1=%d, z1=%d), (x2=%d, y2=%d, z2=%d))"
     b.first_corner.x b.first_corner.y b.first_corner.z b.second_corner.x
     b.second_corner.y b.second_corner.z *)

let get_stack filename =
  In_channel.with_open_text filename In_channel.input_lines
  |> List.map (fun line ->
         let bricks = String.split_on_char '~' line in
         let convert_to_corner x =
           x |> String.split_on_char ','
           |> List.map (fun x -> x |> Int32.of_string |> Int32.to_int)
           |> Array.of_list
         in
         let first_corner = bricks |> List.hd |> convert_to_corner in
         let second_corner =
           bricks |> List.tl |> List.hd |> convert_to_corner
         in
         {
           first_corner =
             {
               x = first_corner.(0);
               y = first_corner.(1);
               z = first_corner.(2);
             };
           second_corner =
             {
               x = second_corner.(0);
               y = second_corner.(1);
               z = second_corner.(2);
             };
         })
  |> List.map (fun brick ->
         if brick.first_corner.z < brick.second_corner.z then brick
         else
           {
             first_corner = brick.second_corner;
             second_corner = brick.first_corner;
           })

module Brick = struct
  type t = brick

  let compare = compare
end

module BrickMap = Map.Make (Brick)

let rec range a b = if a > b then [] else a :: range (a + 1) b
let max_of = List.fold_left (fun acc e -> if acc < e then e else acc) 0

let product xs ys =
  xs |> List.map (fun x -> ys |> List.map (fun y -> (x, y))) |> List.flatten

let create_empty_support_map max_x max_y =
  Array.make_matrix (max_x + 1) (max_y + 1)
    {
      first_corner = { x = 0; y = 0; z = 0 };
      second_corner = { x = max_x; y = max_y; z = 0 };
    }

let settle_bricks stack empty_support_map =
  let settle_bricks_folder (support_map, settled_stack) brick =
    let min_brick_x = min brick.first_corner.x brick.second_corner.x in
    let max_brick_x = max brick.first_corner.x brick.second_corner.x in
    let min_brick_y = min brick.first_corner.y brick.second_corner.y in
    let max_brick_y = max brick.first_corner.y brick.second_corner.y in
    let max_z =
      product (range min_brick_x max_brick_x) (range min_brick_y max_brick_y)
      |> List.map (fun (x, y) ->
             let support_brick = support_map.(x).(y) in
             max support_brick.first_corner.z support_brick.second_corner.z)
      |> max_of
    in
    let z_shift = min brick.first_corner.z brick.second_corner.z - max_z - 1 in
    let settled_brick =
      {
        first_corner =
          { brick.first_corner with z = brick.first_corner.z - z_shift };
        second_corner =
          { brick.second_corner with z = brick.second_corner.z - z_shift };
      }
    in
    product (range min_brick_x max_brick_x) (range min_brick_y max_brick_y)
    |> List.iter (fun (x, y) -> support_map.(x).(y) <- settled_brick);
    (support_map, settled_brick :: settled_stack)
  in
  let z_based_comparison a b = a.first_corner.z - b.first_corner.z in
  let _, setted_stack_in_rev =
    stack
    |> List.sort z_based_comparison
    |> List.fold_left settle_bricks_folder (empty_support_map, [])
  in
  List.rev setted_stack_in_rev

let identify_support settled_stack length width =
  let identify_support_folder
      (brick_to_supported_bricks, brick_to_supporting_bricks, support_map) brick
      =
    let min_brick_x = min brick.first_corner.x brick.second_corner.x in
    let max_brick_x = max brick.first_corner.x brick.second_corner.x in
    let min_brick_y = min brick.first_corner.y brick.second_corner.y in
    let max_brick_y = max brick.first_corner.y brick.second_corner.y in
    let supporting_z = min brick.first_corner.z brick.second_corner.z - 1 in
    let supporting_bricks =
      product (range min_brick_x max_brick_x) (range min_brick_y max_brick_y)
      |> List.filter_map (fun (x, y) ->
             let candidate_brick = support_map.(x).(y) in
             if candidate_brick.second_corner.z == supporting_z then
               Some candidate_brick
             else None)
    in
    let new_brick_to_supporting_bricks =
      supporting_bricks
      |> List.fold_left
           (fun map supporting_brick ->
             BrickMap.update brick
               (function
                 | Some xs ->
                     Some
                       (List.sort_uniq brick_comparator (supporting_brick :: xs))
                 | None -> Some [ supporting_brick ])
               map)
           brick_to_supporting_bricks
    in
    let new_brick_to_supported_bricks =
      supporting_bricks
      |> List.fold_left
           (fun map supporting_brick ->
             BrickMap.update supporting_brick
               (function
                 | Some xs ->
                     Some (List.sort_uniq brick_comparator (brick :: xs))
                 | None -> Some [ brick ])
               map)
           brick_to_supported_bricks
    in
    product (range min_brick_x max_brick_x) (range min_brick_y max_brick_y)
    |> List.iter (fun (x, y) -> support_map.(x).(y) <- brick);
    (new_brick_to_supported_bricks, new_brick_to_supporting_bricks, support_map)
  in
  let supported_map, supporting_map, _ =
    List.fold_left identify_support_folder
      (BrickMap.empty, BrickMap.empty, create_empty_support_map length width)
      settled_stack
  in
  (supported_map, supporting_map)

let solve_part1 stack length width =
  (* let open Printf in *)
  let settled_stack =
    settle_bricks stack (create_empty_support_map length width)
  in
  let brick_to_supported_bricks, brick_to_supporting_bricks =
    identify_support settled_stack length width
  in
  (* BrickMap.iter
       (fun brick bricks ->
         Format.printf "brick %a -> " pp_brick brick;
         bricks |> List.iter (Format.printf "brick %a " pp_brick);
         printf "\n")
       brick_to_supported_bricks;
     BrickMap.iter
       (fun brick bricks ->
         Format.printf "brick %a <- " pp_brick brick;
         bricks |> List.iter (Format.printf "brick %a " pp_brick);
         printf "\n")
       brick_to_supporting_bricks; *)
  settled_stack
  |> List.filter (fun brick ->
         match BrickMap.find_opt brick brick_to_supported_bricks with
         | None -> true
         | Some bricks ->
             bricks
             |> List.for_all (fun b ->
                    BrickMap.find b brick_to_supporting_bricks
                    |> List.length > 1))
  |> List.length

let solve_part2 stack length width =
  let settled_stack =
    settle_bricks stack (create_empty_support_map length width)
  in
  let brick_to_supported_bricks, brick_to_supporting_bricks =
    identify_support settled_stack length width
  in
  let calculate_falling_bricks brick =
    let rec helper supported_bricks acc =
      if List.is_empty supported_bricks then acc
      else
        let bricks_supported_only_by_brick =
          supported_bricks
          |> List.filter (fun supported_brick ->
                 match
                   BrickMap.find_opt supported_brick brick_to_supporting_bricks
                 with
                 | Some bs ->
                     bs
                     |> List.for_all (fun b ->
                            List.find_opt (fun a -> a == b) acc
                            |> Option.is_some)
                 | None -> false)
        in
        let next_acc = List.append acc bricks_supported_only_by_brick in
        let next_supported_bricks =
          bricks_supported_only_by_brick
          |> List.map (fun supporting_brick ->
                 match
                   BrickMap.find_opt supporting_brick brick_to_supported_bricks
                 with
                 | Some bs -> bs
                 | None -> [])
          |> List.flatten
          |> List.sort_uniq brick_comparator
        in
        helper next_supported_bricks next_acc
    in
    (helper
       (BrickMap.find_opt brick brick_to_supported_bricks
       |> Option.value ~default:[])
       [ brick ]
    |> List.sort_uniq brick_comparator
    |> List.length)
    - 1
  in
  List.map calculate_falling_bricks settled_stack |> List.fold_left ( + ) 0

let () =
  let filename = Sys.argv.(1) in
  let open Printf in
  let stack = get_stack filename in
  let length =
    stack
    |> List.map (fun b -> max b.first_corner.x b.second_corner.x)
    |> max_of
  in
  let width =
    stack
    |> List.map (fun b -> max b.first_corner.y b.second_corner.y)
    |> max_of
  in
  printf "%d\n" (solve_part1 stack length width);
  printf "%d\n" (solve_part2 stack length width)
