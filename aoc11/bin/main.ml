open Printf

type monkey = int Queue.t * (int -> int) * (int -> int)

let worry_lim = 2*7*11*19*3*5*17*13

let monkeys : monkey Array.t =
  [([66;59;64;51] |> List.to_seq |> Queue.of_seq,
   (fun x -> x*3),
   fun z -> if z mod 2 == 0 then 1 else 4);
   ([67;61] |> List.to_seq |> Queue.of_seq,
   (fun x -> x * 19),
   fun z -> if z mod 7 == 0 then 3 else 5);
   ([86;93;80;70;71;81;56] |> List.to_seq |> Queue.of_seq,
   (fun x -> x + 2),
   fun z -> if z mod 11 == 0 then 4 else 0);
   ([94] |> List.to_seq |> Queue.of_seq,
   (fun x -> x * x),
   fun z -> if z mod 19 == 0 then 7 else 6);
   ([71;92;64] |> List.to_seq |> Queue.of_seq,
   (fun x -> x + 8),
   fun z -> if z mod 3 == 0 then 5 else 1);
   ([58;81;92;75;56] |> List.to_seq |> Queue.of_seq,
   (fun x -> x + 6),
   fun z -> if z mod 5 == 0 then 3 else 6);
   ([82;98;77;94;86;81] |> List.to_seq |> Queue.of_seq,
   (fun x -> x + 7),
   fun z -> if z mod 17 == 0 then 7 else 2);
   ([54;95;70;93;88;93;63;50] |> List.to_seq |> Queue.of_seq,
   (fun x -> x + 4),
   fun z -> if z mod 13 == 0 then 2 else 0)] |> Array.of_list

let monkey_business : int Array.t = Array.make 8 0

let throw_to_monkey v i =
  let (q,_,_) = monkeys.(i) in Queue.add v q

let monkey_turn i =
  let (q, op, test) = monkeys.(i) in
  let process_item v =
    let v1 = (op v mod worry_lim) in
    let destmonkey = test v1 in
    monkey_business.(i) <- monkey_business.(i) + 1; throw_to_monkey v1 destmonkey in
  let rec runner () =
    if Queue.is_empty q then ()
    else (process_item (Queue.pop q); runner ()) in
  runner ()

let rec monkey_round n =
  match n with 0 -> () | _ ->
  (Seq.ints 0 |> Seq.take 8 |> Seq.iter monkey_turn); monkey_round (n-1)


let two_largest l = l |> Array.to_list |> List.sort Int.compare |> List.rev |> List.to_seq |> Seq.take 2 |> Seq.fold_left Int.mul 1

let () = 
  monkey_round 10000;  Array.iter (printf "%d\n") monkey_business; two_largest monkey_business |> printf "%d\n"