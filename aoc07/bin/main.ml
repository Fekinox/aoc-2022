open Printf

module Mp = Map.Make(String)

type mtree = File of string * int | Dir of (string * int * mtree Mp.t) ref
type lsnode = Lfile of string * int | Ldir of string
type instr = Cdroot | Cdin of string | Cdout | Ls of (lsnode list)

let contents mt =
  match mt with
  | File (_)-> raise (Failure "invalid")
  | Dir cl -> cl

let cost mt =
  match mt with
  | File _ -> raise (Failure "invalid")
  | Dir cl -> let (_,s,_) = !cl in s

let recompute_size mt =
  match mt with
  | File _ -> ()
  | Dir cl ->
    let (n,_,conts) = !cl in
    let addsz _ m s =
      match m with
      | File (_,fs) -> s+fs
      | Dir c -> let (_, ds,_) = !c in s+ds in
    cl := (n, Mp.fold addsz conts 0, conts)



let addlsnode mt l =
  let cs = contents mt in
  let name, _, conts = !cs in
  (* If conts is nonempty we found a new directory*)
  if Mp.is_empty conts
  then
    let build_dir (sz, d) ln =
      match ln with
      | Lfile (s,i) -> (sz+i,Mp.add s (File (s,i)) d)
      | Ldir s -> (sz,Mp.add s (Dir (ref (s, 0, Mp.empty))) d) in
    let (sz, d) = List.fold_left build_dir (0, Mp.empty) l in
    cs := (name, sz, d)
  (* Otherwise we just need to recompute the sizes*)
  else
    recompute_size mt

let rec get_instr () =
  read_line () |> get_instr2
and get_instr2 s =
  (* This instr is guaranteed to start a line *)
  let split = String.split_on_char ' ' s |> List.tl in
  match List.hd split with
  | "cd" ->
    (match List.nth split 1 with
    | "/" -> Seq.Cons(Cdroot, get_instr)
    | ".." -> Seq.Cons(Cdout, get_instr)
    | s -> Seq.Cons(Cdin s, get_instr))
  | _ ->
    (let rec runner files =
      try (let lsline = read_line () in
      match String.split_on_char ' ' lsline with
      | ["dir";dn] -> runner ((Ldir dn)::files)
      | ("$"::_) -> Seq.Cons (Ls (List.rev files), fun _ -> get_instr2 lsline)
      | [value;filename] -> runner ((Lfile (filename, int_of_string value))::files) 
      | _ -> raise (Failure "invalid"))
    with End_of_file -> Seq.Cons(Ls files, fun _ -> Seq.Nil) in
    runner [])

(* let print_instr i =
  match i with
  | Cdroot -> printf "cd /\n"
  | Cdin s -> printf "cd %s\n" s
  | Cdout -> printf "cd ..\n"
  | Ls fs ->
    printf "ls / "; List.iter (fun f -> match f with | Lfile (s,i) -> printf "%s %d / " s i | Ldir s -> printf "dir %s / " s) fs; print_newline () 
   *)
(* let print_tree mt =
  let rec print_treer pref mt =
    match mt with
    | File (s,i) -> printf "%s file %s (%i)\n" pref s i
    | Dir cl ->
    let (s,i,c) = !cl in
      printf "%s dir %s (%i)\n" pref s i;
      Mp.iter (fun _ -> print_treer (pref ^ " - ")) c in
  print_treer "" mt *)

let process_instr (f, mt) i =
  match i with
  | Cdroot -> ([], mt)
  | Cdin d ->
    let cs = contents mt in
    let _, _, conts = !cs in
    let nexttree = Mp.find d (conts) in
    (mt::f, nexttree) 
  | Cdout ->
    (match f with
    | [] -> raise (Failure "invalid")
    | (p::ps) -> (ps, p))
  | Ls lsnodes ->
      addlsnode mt lsnodes;
      List.iter recompute_size f;
      (f,mt)

(* let small_dirs (mt : mtree) =
  let rec runner s mt =
    match mt with
    | File _ -> s
    | Dir cl ->
      let (_,sz,conts) = !cl in
      let ex = if sz <= 100000 then sz else 0 in
      Mp.fold (fun _ -> fun m -> fun cur -> runner cur m) conts (s+ex) in
  runner 0 mt *)

let smallest_dir_to_delete (mt : mtree) =
  let total = 70000000 in
  let req = 30000000 in
  let needed_to_delete = req - (total - cost mt) in
  let _ = printf "%d\n" needed_to_delete in
  let rec runner sm m =
    match m with
    | File _ -> sm
    | Dir cl ->
      let (nm,sz,conts) = !cl in
      let _ = printf "exploring %s\n" nm in
      let _ = printf "min: %d\n" sm in
      let _ = printf "sz: %d\n" sz in
      let newsm = if sz >= needed_to_delete then (printf "smaller\n"; min sm sz) else sm in
      let _ = printf "newsm: %d\n" newsm in
      Mp.fold (fun _ -> fun m -> fun cur -> runner cur m) conts newsm in
  runner (cost mt) mt

let () =
  let intree = Dir (ref ("/", 0, Mp.empty)) in
  let instrs = get_instr in
  let _ = Seq.fold_left process_instr ([], intree) instrs in
  (* print_tree intree; *)
  smallest_dir_to_delete intree |> printf "%d\n"