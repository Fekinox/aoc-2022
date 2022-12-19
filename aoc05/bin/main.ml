open Printf

let add_to_stack sks v i =
  let s = sks.(i) in
  Array.set sks i (v::s)

let pop_from_stack sks i =
  let [@warning "-8"] (s::ss) = sks.(i) in
  Array.set sks i ss; s

let stack_tops (sks : char list array) =
  let mapped = Array.map List.hd sks in
  Array.fold_right (fun c -> fun st -> let as_str = String.make 1 c in as_str ^ st) mapped ""

let process_stacks sts =
  let (_,t) = List.hd sts, List.tl sts in
  let numstacks = 9 in
  let res_stacks : char list Array.t = Array.init numstacks (fun _ -> []) in
  let add_to_stacks s =
    ignore ((List.init numstacks) (fun i -> let c = String.get s (4*i+1) in if c = ' ' then () else add_to_stack res_stacks c i)) in
  List.iter add_to_stacks t; res_stacks

let print_stacks sts =
  Array.iter (fun s -> List.fold_right (fun c -> fun st -> st ^ (String.make 1 c)) s "" |> printf "%s\n") sts

let conv s = printf "\"%s\"\n" s; int_of_string s

let process_inst s =
  let l = Str.split (Str.regexp " ?[a-z]+ ?") s |> List.map conv in
  match l with
  | [a;b;c] -> (a,b-1,c-1)
  | _ -> raise (Failure "bad")
(* 
let rec run_inst sks (a,b,c) =
  match a with
  | 0 -> ()
  | _ ->
    let v = pop_from_stack sks b in
    add_to_stack sks v c; run_inst sks (a-1,b,c) *)
  
let run_inst_order_preserving sks (a,b,c) =
  let rec runner acc a =
    match a with
    | 0 -> acc
    | _ -> runner ((pop_from_stack sks b)::acc) (a-1) in
  let crates = runner [] a in
  List.iter (fun ct -> add_to_stack sks ct c) crates

let () =
  let stacks =
    let rec stacks' () =
        let l = read_line () in
        if String.length l = 0
          then Seq.Nil else Seq.Cons(l, stacks') in
    stacks' |> List.of_seq |> List.rev |> process_stacks in
  let _ = print_stacks stacks in
  let rec insts () =
    try (Seq.Cons (read_line () |> process_inst, insts)) with End_of_file -> Seq.Nil in
  Seq.fold_left (fun _ -> fun i -> run_inst_order_preserving stacks i) () insts; printf "%s\n" (stack_tops stacks)