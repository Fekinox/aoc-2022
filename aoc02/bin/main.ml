open Printf

let () = print_endline "Hello, World!"

(* let rps (opponent, player) =
  match (opponent, player) with
  | ("A","X") -> 4
  | ("A","Y") -> 8
  | ("A","Z") -> 3
  | ("B","X") -> 1
  | ("B","Y") -> 5
  | ("B","Z") -> 9
  | ("C","X") -> 7
  | ("C","Y") -> 2
  | _ -> 6 *)

let rps2 (opponent, player) =
  match (opponent, player) with
  | ("A","X") -> 3
  | ("A","Y") -> 4
  | ("A","Z") -> 8
  | ("B","X") -> 1
  | ("B","Y") -> 5
  | ("B","Z") -> 9
  | ("C","X") -> 2
  | ("C","Y") -> 6
  | _ -> 7

let () =
  let rec runner () =
    try (
      let [@warning "-8"] [a;b] = read_line () |> String.split_on_char ' ' in
      rps2 (a,b) + (runner ())
    ) with End_of_file -> 0 in
  runner () |> printf "%d\n"