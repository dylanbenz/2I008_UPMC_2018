let rec somme_des_chiffres n =
	if n<10 then n
	else (n mod 10) + somme_des_chiffres (n/10);;

let divisible_par_trois n =
	let rec loop n r =
		if n<10 then n+r
		else loop (n/10) (r+(n mod 10))
	in
		(loop n 0) mod 3 = 0;;
