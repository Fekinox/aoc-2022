open Printf

module St = Set.Make(Char)

let () =
  let groups =
    let rec runner () =
      try (Seq.Cons([read_line ();read_line (); read_line ()], runner)) with End_of_file -> Seq.Nil in
    runner in
  let prio x =
    let od = Char.code x in
    if od <= 90
    then od-38
    else od-96 in
  let process_line s =
    let ln = String.length s in
    let first_half, second_half = (String.sub s 0 (ln/2), String.sub s (ln/2) (ln/2)) in
    let toset s = s |> String.to_seq |> St.of_seq in
    (toset first_half, toset second_half) in
  let process_group g =
    let _ = printf "%d\n" (List.length g) in
    let sets = g |> List.map process_line |> List.map (fun (s1,s2) -> St.union s1 s2) in
    let common = List.fold_left St.inter (List.hd sets) (List.tl sets) in
    St.choose common |> prio in
  groups |> Seq.map process_group |> Seq.fold_left Int.add 0 |> printf "%d\n"
  (* lines |> Seq.map process_line |> Seq.fold_left Int.add 0 |> printf "%d\n" *)