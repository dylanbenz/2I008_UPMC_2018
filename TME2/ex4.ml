let rec merge l1 l2 =
  match l1,l2 with
  | [],[]->[]
  | x::xl1,y::yl2->if (compare x y)<0 then x::(merge xl1 l2) else y::(merge l1 yl2)
  | _,[]->l1
  | [],_->l2

let merge l1 l2 =
  let rec loop l1 l2 acc =
    match l1,l2 with
    | [],[] -> acc
    | x::xl1,y::yl2->if (compare x y)<0 then loop xl1 l2 (x::acc) else loop l1 yl2 (y::acc)
    | x::xl1,[]-> loop xl1 [] (x::acc)
    | [],y::yl2-> loop [] yl2 (y::acc)
  in
    List.rev (loop l1 l2 [])

let rec split l =
  let rec loop l l1 l2 i =
    match l with []->(l1,l2)
    | x::xl -> if (i mod 2)=0 then loop xl (x::l1) l2 (i+1) else loop xl l1 (x::l2) (i+1)
  in
    loop l [] [] 0

let rec merge_sort l =
  match l with
  | [] | [_]-> l
  | _ ->
     let (l1,l2) = split l in
     let l1_triee = merge_sort l1 in
     let l2_triee = merge_sort l2 in
     merge l1_triee l2_triee
