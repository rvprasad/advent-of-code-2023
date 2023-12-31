type record = { spring_spans : char list list; damaged_springs : int list }

let ( >> ) f g x = g (f x)

module Record = struct
  type t = record

  let compare = compare
end

module RecordMap = Map.Make (Record)

let parse filename transform1 transform2 =
  let parse_line line =
    let tmp1 = String.split_on_char ' ' line in
    let spring_spans =
      List.nth tmp1 0 |> transform1 |> String.split_on_char '.'
      |> List.filter (fun x -> x <> String.empty)
      |> List.map (String.to_seq >> List.of_seq)
    in
    let damaged_springs =
      List.nth tmp1 1 |> transform2 |> String.split_on_char ','
      |> List.map (String.trim >> Int32.of_string >> Int32.to_int)
    in
    { spring_spans; damaged_springs }
  in
  let lines = In_channel.with_open_text filename In_channel.input_lines in
  lines |> List.map parse_line

let process records =
  let cache = Hashtbl.create 173832 in
  let rec get_count record =
    match Hashtbl.find_opt cache record with
    | Some x -> x
    | None ->
        let ret =
          match record with
          | { spring_spans = s; damaged_springs = [] } ->
              if s |> List.for_all (fun x -> List.for_all (fun x -> x <> '#') x)
              then 1
              else 0
          | { spring_spans = []; damaged_springs = _ :: _ } -> 0
          | { spring_spans = [] :: ss_tl; damaged_springs = ds } ->
              get_count { spring_spans = ss_tl; damaged_springs = ds }
          | { spring_spans = [ '#' ] :: ss_tl; damaged_springs = 1 :: ds_tl } ->
              get_count { spring_spans = ss_tl; damaged_springs = ds_tl }
          | { spring_spans = [ '?' ] :: ss_tl; damaged_springs = 1 :: ds_tl } ->
              get_count { spring_spans = ss_tl; damaged_springs = ds_tl }
              + get_count { spring_spans = ss_tl; damaged_springs = 1 :: ds_tl }
          | {
           spring_spans = ('#' :: e :: ss_hd_tl) :: ss_tl;
           damaged_springs = 1 :: ds_tl;
          } ->
              if e <> '#' then
                get_count
                  { spring_spans = ss_hd_tl :: ss_tl; damaged_springs = ds_tl }
              else 0
          | {
           spring_spans = ('?' :: e :: ss_hd_tl) :: ss_tl;
           damaged_springs = 1 :: ds_tl;
          } ->
              let x =
                if e <> '#' then
                  get_count
                    {
                      spring_spans = ss_hd_tl :: ss_tl;
                      damaged_springs = ds_tl;
                    }
                else 0
              in
              let y =
                get_count
                  {
                    spring_spans = (e :: ss_hd_tl) :: ss_tl;
                    damaged_springs = 1 :: ds_tl;
                  }
              in
              x + y
          | { spring_spans = ss_hd :: ss_tl; damaged_springs = n :: ds_tl }
            when n = List.length ss_hd
                 && List.for_all (fun x -> x = '#' || x = '?') ss_hd ->
              let x =
                get_count { spring_spans = ss_tl; damaged_springs = ds_tl }
              in
              x
              +
              if List.for_all (fun x -> x = '?') ss_hd then
                get_count
                  {
                    spring_spans = List.tl ss_hd :: ss_tl;
                    damaged_springs = n :: ds_tl;
                  }
              else 0
          | { spring_spans = ss_hd :: _; damaged_springs = n :: _ }
            when n > List.length ss_hd && List.exists (fun x -> x = '#') ss_hd
            ->
              0
          | { spring_spans = ss_hd :: ss_tl; damaged_springs = n :: ds_tl }
            when n > List.length ss_hd && List.for_all (fun x -> x = '?') ss_hd
            ->
              get_count { spring_spans = ss_tl; damaged_springs = n :: ds_tl }
          | { spring_spans = ss_hd :: ss_tl; damaged_springs = n :: ds_tl }
            when n < List.length ss_hd ->
              let x =
                if
                  ss_hd
                  |> List.filteri (fun i _ -> i < n)
                  |> List.for_all (fun x -> x = '#' || x = '?')
                  && List.nth ss_hd n <> '#'
                then
                  get_count
                    {
                      spring_spans =
                        (ss_hd |> List.filteri (fun i _ -> i > n)) :: ss_tl;
                      damaged_springs = ds_tl;
                    }
                else 0
              in
              let y =
                if List.hd ss_hd <> '#' then
                  get_count
                    {
                      spring_spans = List.tl ss_hd :: ss_tl;
                      damaged_springs = n :: ds_tl;
                    }
                else 0
              in
              x + y
          | _ -> failwith "Invalid state"
        in
        Hashtbl.add cache record ret;
        ret
  in
  records |> List.map get_count |> List.fold_left ( + ) 0

let () =
  let filename = Sys.argv.(1) in
  let open Printf in
  let id x = x in
  let transform1 x = String.concat "?" (Array.make 5 x |> Array.to_list) in
  let transform2 x = String.concat "," (Array.make 5 x |> Array.to_list) in
  printf "%d\n" (process (parse filename id id));
  printf "%d\n" (process (parse filename transform1 transform2))
