let rec p n =
  if n=1 then 1.
  else (1. /. float_of_int(n*n) +. p (n-1))
;;

let p2 n =
  let rec loop n r =
    if n=1 then r
    else loop (n-1) (1. /. float_of_int(n*n) +. r)
  in
  loop n 1.
;;
