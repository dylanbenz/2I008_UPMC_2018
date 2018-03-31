type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree
			       		       
let rec hauteur abr =
  match abr with
  | Empty -> 0
  | Node(x,g,d)->1+max (hauteur g) (hauteur d)

let rec taille abr =
  match abr with
  | Empty -> 0
  | Node(x,g,d) -> 1 + taille g + taille d

let rec assoc abr alpha =
  match abr with
  | Empty -> None
  | Node((a,b),g,d)-> if a=alpha
		      then Some(b)
		      else assoc g alpha; assoc d alpha
		    
let rec to_list abr =
  match abr with
  | Empty -> []
  | Node(x,g,d) -> [x]@(to_list g)@(to_list d)

let rec insert abr x =
  match abr with
  | Empty -> Node(x,Empty,Empty)
  | Node(e,g,d) -> if x<=e
		   then Node(e, insert g x, d)
		   else Node(e, g, insert d x)
		     
		   
type value =
  | Integer of int
  | Boolean of bool

type expr =
  | Value of value
  | Mult of expr*expr
  | Add of expr*expr
  | Autre of expr*expr*expr

let rec eval e =
  match e with
  | Value(a) -> a
  | Mult(a,b) ->
     begin
       match eval a,eval b with
       | Integer(a),Integer(b) -> Integer(a * b)
       | Boolean(a),Boolean(b) -> Boolean(a && b)
       | _ -> failwith "type error"
     end
  | Add(a,b) ->
     begin
       match eval a,eval b with
       | Integer(a),Integer(b) -> Integer(a + b)
       | Boolean(a),Boolean(b) -> Boolean(a || b)
       | _ -> failwith "type error"
     end
  | Autre(a,b,c) ->
     begin
       match eval a, eval b, eval c with
       | Boolean(a), Integer(b), Integer(c) -> if a then Integer(b) else Integer(c)
       | Boolean(a), Boolean(b), Boolean(c) -> if a then Boolean(b) else Boolean(c)
       | _ -> failwith "type error"
     end

    
type card =
  | As
  | Roi
  | Dame
  | Valet
  | Autre of int

let point_of_card c =
  let value = 
    match c with
    | As -> 150
    | Roi -> 100
    | Dame -> 80
    | Valet -> 50
    | Autre(x) -> x
  in
  (-1)*value,value
