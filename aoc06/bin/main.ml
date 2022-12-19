open Printf

module St = Set.Make(Char)

let first_all_diff k s =
  let rec runner i =
    if (i >= String.length s) then raise (Failure "impossible")
    else
      printf "%s\n" (String.sub s (i-k+1) k); if (String.sub s (i-k+1) k |> String.to_seq |> St.of_seq |> St.cardinal |> Int.equal k) then (i+1) else runner (i+1) in
  runner (k-1)

let () =
  read_line () |> first_all_diff 14 |> printf "%d\n"