type value = Integer of int
	   | Boolean of bool

type expr = Value of value
          | Add of expr*expr
	  | Mult of expr*expr
	  | Test of expr*expr*expr

let rec eval (e:expr) : value =
  match e with
  | Value(e) -> e
  | Add(e1,e2) -> 
     begin
       match eval e1,eval e2 with
       | Integer(e1),Integer(e2) -> Integer(e1 + e2)
       | Boolean(e1),Boolean(e2) -> Boolean(e1 || e2)
       | _,_ -> raise (Invalid_argument "type error")
     end
  | Mult(e1,e2) ->
     begin
       match eval e1, eval e2 with
       | Integer(e1),Integer(e2) -> Integer(e1 * e2)
       | Boolean(e1),Boolean(e2) -> Boolean(e1 && e2)
       | _,_ -> raise (Invalid_argument "type error")
     end 
  | Test(e1, e2, e3) ->
     begin
       match eval e1 with
       | Boolean(e1) -> if e1 then eval e2 else eval e3
       | Integer(e1) -> if e1<>0 then eval e2 else eval e3
     end
