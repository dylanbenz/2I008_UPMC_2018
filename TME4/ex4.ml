type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree 

let rec insert x abr =
  match abr with
  | Empty -> Node(x, Empty, Empty)
  | Node(e, g, d) -> if x<=e then Node(e, insert x g, d)
		     else Node(e, g, insert x d)

let from_list liste =
  List.fold_left (fun abr x -> insert x abr) Empty liste

let rec mem t x =
  match t with
  | Empty -> false
  | Node(e, g, d) -> if e=x then true
		     else if x<e then mem g x
		     else mem d x

let rec to_list t =
  match t with
  | Empty -> []
  | Node(e, g, d) -> (to_list g)@[e]@(to_list d)

let tri l =
  to_list (from_list l)

	 
