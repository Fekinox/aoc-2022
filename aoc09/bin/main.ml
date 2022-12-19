open Printf

module IntPairs =
struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c
end

module St = Set.Make(IntPairs)

let parse_instr (s : string) : (int * int) * int =
  let [@warning "-8"] [dir;steps] = String.split_on_char ' ' s in
  let (dr,dc) =
  match dir with
  | "D" -> (1,0)
  | "U" -> (-1,0)
  | "L" -> (0,-1)
  | _ -> (0,1) in
  ((dr,dc),int_of_string steps)

let rec gen_instrs () =
  try (
    let (dir, count) = read_line () |> parse_instr in
    Seq.Cons (dir, Seq.append (Seq.repeat dir |> Seq.take (count-1)) gen_instrs)
  ) with End_of_file -> Seq.Nil

let rec print_rope rp =
  match rp with
  | [] -> raise (Failure "empty")
  | [(x,y)] -> printf "(%d,%d)\n" x y
  | (x,y)::r -> printf "(%d,%d), " x y; print_rope r

let step (vis_set, rope) (dr, dc) =
  let _ = printf "moving %d %d\n" dr dc in
  let _ = print_rope rope in
  let rec runner rope =
    match rope with
    | (xr,xc)::(yr,yc)::rest ->
      (* Compute distances *)
      let ydist, xdist = yr-xr, yc-xc in
      (* If touching, don't move *)
      if (abs ydist <= 1) && (abs xdist <= 1)
      then let (v, r) = runner ((yr,yc)::rest) in
           (v, (xr,xc)::r)
      else
        let ysgn = compare ydist 0 in
        let xsgn = compare xdist 0 in
        let y' = (yr-ysgn, yc-xsgn) in
        let (v, r) = runner (y'::rest) in
        (v, (xr,xc)::r)
    | [x] -> (St.add x vis_set, [x])
    | _ -> raise (Failure "bad") in
  match rope with
  | [] -> raise (Failure "bad")
  | ((h0,h1)::hs) ->
    let h' = h0+dr, h1+dc in
    runner (h'::hs)

let () =
  let (vis,_) = gen_instrs |>
  Seq.fold_left step (St.empty, List.init 10 (fun _ -> (0,0))) in
  St.cardinal vis |> printf "%d\n"