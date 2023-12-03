type draw = {
  red: int;
  green: int;
  blue: int;
}

let get_content filename =
  let process_game_info game_line =
    let game_id_and_draws = String.split_on_char ':' game_line in
    let convert_draw draw =
      let freq_and_colors = List.map (fun x -> String.split_on_char ' ' @@ String.trim x) draw in
      let helper c = function
          | freq :: color :: [] when color = c -> Some(int_of_string freq)
          |  _ -> None in
      let get_freq c =
        let tmp1 = List.filter_map (helper c) freq_and_colors in
        if List.is_empty tmp1 then 0 else List.hd tmp1
      in
      {red = (get_freq "red"); green = get_freq "green"; blue = get_freq "blue"}
    in
    let draws = List.nth game_id_and_draws 1 |>
      String.split_on_char ';' |>
      List.map (String.split_on_char ',') |>
      List.map convert_draw
    in
    let game_id =
      let tmp1 = List.hd game_id_and_draws |> (String.split_on_char ' ') in
      List.nth tmp1 1 |> int_of_string in
    (game_id, draws)
  in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  List.map process_game_info lines

let process1 content r g b =
  let open Stdio in
  let is_possible (game_id, draws) =
    let flag = draws |> List.map (function
      | {red; green; blue} when r >= red && g >= green && b >= blue -> true
      | _ -> false) |> List.fold_left (&&) true
    in
    if flag then game_id else 0
  in
  content |> List.map is_possible |> List.fold_left ( + ) 0 |> printf "one %i\n\n"

let process2 content =
  let open Stdio in
  let power_of_game(_, draws) =
    let helper (r, g, b) a = ((max a.red r), (max a.green g), (max a.blue b)) in
    let (r, g, b) = draws |> List.fold_left helper (0, 0, 0) in
    r * g * b
  in
  content |> List.map power_of_game |> List.fold_left ( + ) 0 |> printf "two %i\n"

let () =
  let filename = Sys.argv.(1) in
  let content = get_content filename in
  let red = int_of_string Sys.argv.(2) in
  let green = int_of_string Sys.argv.(3) in
  let blue = int_of_string Sys.argv.(4) in
  process1 content red green blue ;
  process2 content


