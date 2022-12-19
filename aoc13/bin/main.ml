open Printf

type token =
  | Number of int
  | Comma
  | Open
  | Close

type packet = Num of int | Lst of packet list

let tokenize (s : string) =
  let splits = Str.full_split (Str.regexp {|[<,>]|}) s in
  let mapfn tk = match tk with
  | Str.Delim "<" -> Open
  | Str.Delim ">" -> Close
  | Str.Delim _ -> Comma
  | Str.Text s -> Number (int_of_string s) in
  List.map mapfn splits

let rec parse (tk : token list) : (packet * token list) =
  match tk with
  | (Number n)::tk' -> (Num n, tk')
  | Open::tk' ->
    (let rec runner vl tks =
      match tks with
      | Close::tks' -> (Lst (List.rev vl), tks')
      | Comma::tks' -> runner vl tks'
      | _ ->
        let (v, tks') = parse tks in runner (v::vl) tks' in
      runner [] tk')
  | _ -> raise (Failure "bad parse")

let print_packet p =
  let rec print' p =
    match p with
    | Num n -> printf "%d " n
    | Lst l -> printf "["; List.iter print' l; printf "]"; in
  print' p; print_newline ()

let rec in_order p1 p2 sc fc =
  print_packet p1; printf "versus \n"; print_packet p2;
  match (p1, p2) with
  | (Num n1, Num n2) -> 
    (match compare n1 n2 with 
     | -1 -> sc true
     | 0 -> fc ()
     | _ -> sc false)
  | (Lst l1, Lst l2) ->
    (match (l1, l2) with
    | ([], []) -> fc ()
    | ([], _ ) -> sc true
    | (_, []) -> sc false
    | (x::xs, y::ys) ->
      in_order x y sc
        (fun _ -> in_order (Lst xs) (Lst ys) sc fc))
  | (Num n1, Lst _) -> in_order (Lst [Num n1]) p2 sc fc
  | (Lst _, Num n2) -> in_order p1 (Lst [Num n2]) sc fc

let pairs_in_order (p1,p2) =
in_order p1 p2 Fun.id (fun _ -> raise (Failure "shouldn't fail in global scope"))

(* let rec get_packet_pairs () =
  try (
  let (pack1,_) = read_line () |> tokenize |> parse in
  let (pack2,_) = read_line () |> tokenize |> parse in
  let _ = read_line () in
  (* print_packet pack1; print_packet pack2; *)
  Seq.Cons ((pack1,pack2), get_packet_pairs)) with End_of_file -> Seq.Nil *)

let rec get_packets () =
  try (
    let ln = read_line () in
    if String.length ln == 0
    then get_packets ()
    else let (p,_) = ln |> tokenize |> parse in
    Seq.Cons(p, get_packets)
  ) with End_of_file -> Seq.Nil

let () =
  let div1 = Lst ([Lst [Num 2]]) in
  let div2 = Lst ([Lst [Num 6]]) in
  let packs = get_packets |> List.of_seq in
  let smaller_than_one = packs |> List.filter (fun p -> pairs_in_order (p, div1)) |> List.length in
  let smaller_than_two = packs |> List.filter (fun p -> pairs_in_order (p, div2)) |> List.length in
  ((smaller_than_one+1)*(smaller_than_two+2)) |> printf "%d\n"