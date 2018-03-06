type ('a, 'b) gtree =
  | Node of 'a * ('b*('a,'b)gtree)list

type dict = (bool, char) gtree

let empty_dict = Node(true,[])
		       
let is_leaf t =
  
				  
