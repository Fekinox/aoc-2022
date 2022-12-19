open Printf

(* type instr = int *)

(* let update v cyc m =
  if List.mem cyc [20;60;100;140;180;220]
  then (v*cyc)::m
  else m *)

let parse_instr s =
  match String.split_on_char ' ' s with
  | ["noop"] -> fun () -> Seq.Cons(0, fun () -> Seq.Nil)
  | ["addx";n] -> fun () -> Seq.Cons(0, fun () -> Seq.Cons((int_of_string n), fun () -> Seq.Nil))
  | _ -> raise (Failure "bad")

let rec instrs () =
  try (Seq.append (read_line () |> parse_instr) instrs ()) with End_of_file -> Seq.Nil
(* 
let get_sig_strengths instrs =
  let runner (acc, cyc, sigs) ins = (acc+ins,cyc+1,update acc cyc sigs) in
  Seq.fold_left runner (1,1,[]) instrs *)

let () =
  let gridw, gridh = 40, 6 in
  let gridi r c = r*gridw + c in
  let wrunner = Seq.ints 0 |> Seq.take gridw in
  let hrunner = Seq.ints 0 |> Seq.take gridh in
  let grid = Array.make (gridw*gridh) " " in
  let _ =
    let runner (acc, cyc) ins =
      let pos = (cyc-1) mod 40 in
      (* If acc is within 1 pixel of pos, draw the pixel *)
      let _ = if abs (acc-pos) <= 1 then grid.(cyc) <- "#" else () in
      (* Afterwards, if the instruction is an addx update acc accordingly *)
      (acc+ins, cyc+1) in
    Seq.fold_left runner (1,1) instrs in
  Seq.iter (fun r ->
    Seq.iter (fun c -> printf "%s" (grid.(gridi r c))) wrunner; print_newline ()) hrunner
  (* let (_,_,sigs) = get_sig_strengths instrs in *)
  (* let _ = List.iter (printf "%d\n") sigs in *)
  (* List.fold_left Int.add 0 sigs |> printf "%d\n" *)