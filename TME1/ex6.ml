let rec u n =
	if n=0 then 42
	else 3*(u (n-1)) +4;;

let u2 n =
	let rec loop n r =
		if n=0 then r
		else loop (n-1) (3*r +4)
	in
	if n<0 then raise (Invalid_argument "u")
	else loop n 42;;
