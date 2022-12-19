open Printf

let in_range (l,r) x = l <= x && x <= r

let overlapping (((l1,r1) as ell), ((l2,r2) as arr)) =
  (in_range ell l2) || (in_range arr l1) ||
  (in_range ell r2) || (in_range arr r1)

let process_line l =
  let [@warning "-8"] [li;ri] = String.split_on_char ',' l in
  let str_to_pair s =
    let [@warning "-8"] [l;r] = String.split_on_char '-' s in
    (int_of_string l, int_of_string r) in
  let res = (str_to_pair li, str_to_pair ri) in
  overlapping res

let () =
  let rec lines () =
    try (Seq.Cons(read_line () |> process_line, lines)) with End_of_file -> Seq.Nil in
  lines |> Seq.fold_left (fun c -> fun b -> if b then c+1 else c) 0 |> printf "%d\n"