open Printf
open Stdlib

let rec take n xs =
  match (xs, n) with
  | ([], _) -> []
  | (zs, 0) -> []
  | (z::zs, n) -> z::(take (n-1) zs)

let () =
  let add_top_three l z = z::l |> List.sort Int.compare |> List.rev in
  let rec runner topThree z =
    try (
      match read_int_opt () with
      | Some n -> runner topThree (z+n)
      | None -> runner (add_top_three topThree z) 0
    ) with
    End_of_file -> add_top_three topThree z in
  printf "%d\n" (runner [] 0 |> take 3 |> List.fold_left Int.add 0)
