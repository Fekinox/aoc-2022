open Printf

let () =
  let lines =
    let rec lines' () =
      try (Seq.Cons (read_line () |> String.to_seq, lines')) with End_of_file -> Seq.Nil in
    lines' |> List.of_seq in
  let width = List.hd lines |> Seq.length in
  let height = List.length lines in
  let _ = printf "%d x %d\n" width height in
  let grid = Array.make (width*height) 0 in
  (* let visgrid = Array.make (width*height) false in *)
  let get r c g = g.(r*width + c) in
  let set r c v g = g.(r*width + c) <- v in
  (* let markvis r c = set r c true visgrid in *)
  let chartonum ch = (Char.code ch) - (Char.code '0') in
  let out_of_bounds r c =
    not (0 <= r && r < height && 0 <= c && c < width) in
  (* let omax v o =
    match o with
    | None -> Some v
    | Some v' -> Some (max v v') in *)
  let _ =
    List.iteri (fun r -> fun sq ->
      Seq.iteri (fun c -> fun ch -> set r c (chartonum ch) grid) sq) lines in

  (*
  let oprint o = if Option.is_some o then (o |> Option.get |> Int.to_string) else "none" in

  let update_visibles sr sc dr dc =
    let rec runner m r c =
      printf "updating %d %d %s\n" r c (oprint m);
      if out_of_bounds r c then  ()
      else
        (* If current point is above max, it is visible*)
        let nmax = omax (get r c grid) m in
        (* printf "%s %s\n" (oprint nmax) (oprint m); *)
        if nmax <> m
        then (printf "found vis\n"; markvis r c; runner nmax (r+dr) (c+dc))
        else runner nmax (r+dr) (c+dc) in
    runner None sr sc in

  let num_visibles () =
    Array.fold_left (fun sm -> fun b -> if b then sm+1 else sm) 0 visgrid in *)

  let scene_score sr sc =
    let peak = get sr sc grid in
    let _ = printf "peak: %d\n" peak in
    let rec runner r c dr dc count =
      if out_of_bounds r c then count
      else (printf "looking: %d\n" (get r c grid); if (get r c grid) >= peak then (count+1)
      else runner (r+dr) (c+dc) dr dc (count+1)) in
    let leftscore = printf "left\n";runner sr (sc-1) 0 (-1) 0 in
    let rightscore = printf "right\n";runner sr (sc+1) 0 (1) 0 in
    let upscore = printf "up\n";runner (sr-1) sc (-1) 0 0 in
    let downscore = printf "down\n";runner (sr+1) sc 1 0 0 in
    leftscore * rightscore * upscore * downscore in

  let widthrunner = Seq.ints 0 |> Seq.take width in
  let heightrunner = Seq.ints 0 |> Seq.take height in
  (* Left visibles *)
  (* let _ = Seq.iter (fun h -> update_visibles h 0 0 1) heightrunner in
  let _ = Seq.iter (fun h -> update_visibles h (width-1) 0 (-1)) heightrunner in
  let _ = Seq.iter (fun w -> update_visibles 0 w 1 0) widthrunner in
  let _ = Seq.iter (fun w -> update_visibles (height-1) w (-1) 0) widthrunner in *)
  let max_scene_score =
    Seq.fold_left (fun osc -> fun i ->
      Seq.fold_left (fun isc -> fun j -> Int.max (scene_score i j) isc) osc widthrunner) 0 heightrunner in
 
  max_scene_score |> printf "%d\n"