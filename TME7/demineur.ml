type contenu = Mine | Numero of int
type case = (contenu * bool)
type grille = case array array

let affiche_case c =
  match c with
  | (Mine,true) -> print_char ('X')
  | (Numero(i),true) -> print_int (i)
  |  _ -> print_char ('-')

let affiche_grille g =
  let len_d1=Array.length g in
  let len_d2=Array.length g.(0) in

  for i=0 to (len_d1-1) do
    for j=0 to (len_d2-1) do
      affiche_case g.(i).(j)
    done;
      print_char ('\n')
  done

let grille_vide n m = Array.make_matrix n m (Numero(0),false)
  
let ajouter_mine (i,j) g =
  let c = g.(i).(j) in
  match c with
  | (Mine,_) -> false
  | (_,b) -> (g.(i).(j)<-(Mine, b); true)

let random_pos g =
  let len_d1=Array.length g in
  let len_d2=Array.length g.(0) in
  (Random.int len_d1, Random.int len_d2)

let genere_grille (a,b) h l =
  let grille = grille_vide h l in
  let nbMine = (h*l)/10 in
  let rec loop nb =
    if nb<nbMine
    then
      let pos=random_pos grille in
      if pos<>(a,b)
      then
	if ajouter_mine pos grille
	then loop (nb+1)
	else loop nb
      else loop nb
    else grille
  in
  loop 0

let voisins (i,j) g =

  (* let voisins = [ *)
  (*   (i-1),(j-1); (i;j-1) ... *)
  (* ] *)

  (* in *)
  (* List.filter (fun (i,j)-> i >= 0 && i< Array.length g && j >= 0 && j<Array.length g.(0) ) voisins  	*)

let liste_v = ref [] in
  for a=(-1) to 1 do
    for b=(-1) to 1 do
      if (a=0 && b=0 && a+i>=0 && a+i<Array.length g && b+j>=0 && b+j<Array.length g.(0))
      then ()
      else liste_v:=(a+i, b+j)::!liste_v
    done;
  done
    
let numerote_grille g =
  
