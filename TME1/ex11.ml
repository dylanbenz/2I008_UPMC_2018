let mystere = Random.self_init (); Random.int 1000;;

let rec plus_moins () =
  let choix = read_int () in
  let unknown = mystere in

  if choix = unknown then print_string "correct!"
  else if choix < unknown then 
    begin
	  print_string "trop petit!";
	  plus_moins ()
    end
  else
  	begin
	  print_string "trop grand!";
	  plus_moins ()
	end
;;