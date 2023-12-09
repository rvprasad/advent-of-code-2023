type hand_data = { cards : char list; bid : int }

let parse filename =
  let parse_hand line =
    match
      line |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0)
    with
    | [ hand; bid ] ->
        let cards = hand |> String.to_seq |> List.of_seq in
        { cards; bid = Int32.of_string bid |> Int32.to_int }
    | _ -> failwith "Invalid input"
  in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  List.map parse_hand lines

let calculate_winnings hands list_of_freqs_in_decreasing_order card_rank =
  let calculate_hand_ranks hand =
    List.map
      (fun x -> List.find_index (fun y -> x == y) card_rank |> Option.get)
      hand.cards
  in
  let comparator (a, a_freqs_in_decreasing_order)
      (b, b_freqs_in_decreasing_order) =
    let compare_freqs a b =
      let non_zero_diffs =
        Seq.zip (List.to_seq a) (List.to_seq b)
        |> Seq.map (fun (x, y) -> x - y)
        |> Seq.filter (fun x -> x != 0)
      in
      match Seq.uncons non_zero_diffs with None -> 0 | Some (x, _) -> x
    in
    let freq_comparison =
      compare_freqs a_freqs_in_decreasing_order b_freqs_in_decreasing_order
    in
    let compare_card_ranks a b =
      List.combine a b
      |> List.map (fun (x, y) -> x - y)
      |> List.filter (fun x -> x != 0)
      |> List.hd
    in
    let a_ranks = calculate_hand_ranks a in
    let b_ranks = calculate_hand_ranks b in
    if freq_comparison == 0 then compare_card_ranks a_ranks b_ranks
    else freq_comparison
  in
  let ranks = Seq.ints 1 |> Seq.take (List.length hands) |> List.of_seq in
  let hand_ranks =
    List.combine
      (List.sort comparator
         (List.combine hands list_of_freqs_in_decreasing_order))
      ranks
  in
  hand_ranks
  |> List.map (fun ((hand, _), rank) -> hand.bid * rank)
  |> List.fold_left ( + ) 0

let calculate_card_freqs1 hand_list =
  let helper hand =
    hand.cards
    |> List.sort_uniq (fun x y -> Char.code x - Char.code y)
    |> List.map (fun x ->
           hand.cards |> List.filter (fun y -> y == x) |> List.length)
    |> List.sort (fun a b -> b - a)
  in
  hand_list |> List.map helper

let calculate_card_freqs2 hand_list =
  let helper hand =
    let card_freq =
      hand.cards
      |> List.sort_uniq (fun x y -> Char.code x - Char.code y)
      |> List.map (fun x ->
             (x, hand.cards |> List.filter (fun y -> y == x) |> List.length))
      |> List.sort (fun a b -> snd b - snd a)
    in
    let joker_freqs, no_joker_freqs =
      card_freq |> List.partition (fun x -> fst x == 'J')
    in
    let new_card_freqs =
      match (joker_freqs, no_joker_freqs) with
      | [], _ -> no_joker_freqs
      | _, [] -> joker_freqs
      | (_, joker_freq) :: [], (card, freq) :: ys ->
          (card, freq + joker_freq) :: ys
      | _ -> failwith "Invalid input"
    in
    new_card_freqs |> List.split |> snd
  in
  hand_list |> List.map helper

let () =
  let filename = Sys.argv.(1) in
  let hand_data = parse filename in
  let open Printf in
  printf "%d\n"
    (calculate_winnings hand_data
       (calculate_card_freqs1 hand_data)
       [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]);
  printf "%d\n"
    (calculate_winnings hand_data
       (calculate_card_freqs2 hand_data)
       [ 'J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A' ])
