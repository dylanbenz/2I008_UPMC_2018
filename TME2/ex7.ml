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






let char_list_of_string str =
  let rec aux i acc =
    match i with
    | -1 -> acc
    | _ -> aux (i-1) (str.[i] :: acc)
  in
    aux (String.length str - 1) []

let compare_char c1 c2 =
  (int_of_char c1) - (int_of_char c2)
  
let rec list_compare f l1 l2 =
  match l1,l2 with
  | [],[]->0
  | _,[]->1
  | [],_->(-1)
  | x::sl1,y::sl2->if (f x y)=0 then (list_compare f sl1 sl2) else (f x y) 

let is_palindrome s =
  let sl = char_list_of_string s in
  let srev = List.rev (sl) in
  list_compare (compare_char) (sl) srev = 0

let is_anagram s1 s2 =
  let ls1 = char_list_of_string s1 in
  let ls2 = char_list_of_string s2 in
  list_compare (compare_char) (merge_sort ls1) (merge_sort ls2) = 0
