let rec u n=
match n with
  | 0 -> 42
  | _ -> 3* u (n-1) + 4

let u2 n=
  let rec loop n acc =
    match n with 
      | 0 -> acc
      | _ -> loop (n-1) (3*acc + 4)
  in
  loop n 42

let max_list l=
  let rec loop l acc=
    match l with
      | [] -> acc
      | x::xs -> if x > acc then loop xs x else loop xs acc
  in
  match l with
    | [] -> failwith "liste vide"
    | x::xs -> loop xs x 

let rec merge l1 l2 =
  match l1,l2 with
    | [],[]->[]
    | [],l2->l2
    | l1,[]->l1
    | x::xs,y::ys-> x::y::(merge xs ys)
