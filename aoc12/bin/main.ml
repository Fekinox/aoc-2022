open Printf

module Mp = Map.Make(Int)
module St = Set.Make(Int)

let starts = ref []

let dest = ref 0

let height ch =
  match ch with
  | 'S' -> 0
  | 'E' -> Char.code 'z' - Char.code 'a'
  | c -> Char.code c - Char.code 'a'
let lines =
  let rec runner () =
    try (Seq.Cons(read_line () |> String.to_seq |> Array.of_seq, runner)) with End_of_file -> Seq.Nil in
  runner |> Array.of_seq
let w = lines.(0) |> Array.length
let h = Array.length lines

let lines =
  Array.init (w*h) (fun i ->
    let r, c = i/w, i mod w in
    let ch = lines.(r).(c) in
    let _ =
      match ch with
      | ('S'|'a') -> starts := i::(!starts)
      | 'E' -> dest := i
      | _ -> () in
    height ch)

let val_at (r, c) =
  if (0 <= r && r < h) && (0 <= c && c < w)
  then (Some (r,c,lines.(r*w + c))) else None

let build_graph () =
  let runner i =
    let r,c = i/w,i mod w in
    let cur_ht = (lines.(r*w + c)) in
    let nbors = [(r+1,c);(r-1,c);(r,c+1);(r,c-1)] |>
    List.filter_map val_at |>
    List.filter (fun (_,_,w') -> cur_ht - w' <= 1) in
    nbors in
  let gph = Array.init (w*h) runner in
  (gph, w)

let bfs (g, w) st =
  let rec bfs' d vis front =
  if Mp.exists (fun i -> fun _ ->
    lines.(i) == 0) vis
  then d else
  if St.is_empty front then raise (Failure "bad")
  else
    let new_vis =
      St.fold (fun vt -> fun vs ->
        if Mp.mem vt vs then vs else Mp.add vt d vs) front vis in
    let new_front =
      St.fold (fun vt -> fun ft ->
        let nbs = g.(vt) in
        List.fold_left (fun f -> fun (r,c,_) ->
          St.add (r*w + c) f) ft nbs) front St.empty in
    bfs' (d+1) new_vis new_front in
  bfs' 0 Mp.empty (St.singleton st)

let () =
  let (g,w) = build_graph () in
  bfs (g,w) (!dest) |> printf "%d\n"