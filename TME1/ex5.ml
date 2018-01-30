let rec factorielle n =
	if n=0 then 1
	else n*factorielle (n-1);;

let factorielle2 n =
	let rec loop n r =
		if n=0 then r
		else loop (n-1) (n*r)
	in
		loop n 1;;