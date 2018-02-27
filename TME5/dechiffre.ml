let rec occurrence ch freqs =
  match freqs with
  | (caractere,frequence)::fs -> if caractere = ch then (caractere,frequence+1)::fs
				 else (caractere,frequence)::(occurrence ch fs)
  | [] -> [(ch,1)]

let est_minuscule ch = ch>='a' && ch <='z'

let input_char_opt i =
  try
    let c = input_char i in
    Some c
  with
  | End_of_file -> None
					 
let frequences_fichier i =
  let rec loop i acc =
    match (input_char_opt i) with
    | Some c -> if est_minuscule c then loop i (occurrence c acc) else loop i acc 
    | None -> acc
  in
  loop i []
	      
let devine_decalage l =
  match l with
  | [] -> failwith "fichier vide"
  | (c, f) :: ls ->
     begin
       let (i,j) =
	 List.fold_left
	   (fun acc tuple ->
	    match acc, tuple with
	    | (a, b), (c, f) ->
	       if b < f
	       then (c,f)
	       else (a,b))
	   (c, f)
	   ls
       in
       int_of_char i - int_of_char 'e'
     end
       
let dechiffre_char d ch =
  char_of_int((((int_of_char ch - int_of_char 'a')- d)+26) mod 26 + int_of_char 'a')
