(*********)
(* types *)
(*********)

type coord = int * int

type cible = coord

type rayon = int

type cellule = coord * cible * rayon

type monde = cellule list * cellule

(***************)
(* utilitaires *)
(***************)

let rand_point maxx maxy = (Random.int maxx),(Random.int maxy)

let dist (a,b) (c,d) =
  let ac = a-c in
  let bd = b-d in
  sqrt (float_of_int (ac*ac + bd*bd))

let lisse_deplacement (dx,dy) =
  let norme = sqrt (float_of_int ((dx*dx) + (dy*dy))) /. 4. in
  int_of_float ((float_of_int dx)/.norme), int_of_float (((float_of_int dy)/.norme))

(********************)
(* code à completer *)
(********************)

let deplace (((x,y),(a,b),r):cellule) : cellule =
  let dx = a-x in
  let dy = b-y in
  let nx,ny = lisse_deplacement(dx,dy) in
  if dist (x+nx,y+ny) (a,b) <= float_of_int r then (x+nx,y+ny),(rand_point 800 800),r
  else (x+nx,y+ny),(a,b),r

let peut_manger ((c1,_,r1):cellule) ((c2,_,r2):cellule) =
  let d = int_of_float (dist c1 c2)-r1-r2 in
  d<=0 && r1<r2

let deplace_tous ((cells,(coord,old,rad)):monde) (point_joueur:coord) : monde =
  (List.map deplace cells),deplace (coord,point_joueur,rad)

(* prend un monde et retourne un monde ou toute les cellules mangées ce tour-ci
   ont été retirées 
let retirer_mangees ((cells,joueur):monde) : cellule list =
  let cells_mangee = List.filter peut_manger cells in
  List.filter not List.exists cells_mangee cells  
 *)
(* prend une cellule 'c' et une liste de cellules potentiellement mangeable
   et retourne 'c' apres qu'elle ait absorber les cellules plus petites qu'elle
   qui sont à portée *)
let mange (((coord,cible,r) as c):cellule) (cells:cellule list) : cellule =
  (* let function_a_deux_argument a b = a+b in *)
  (* List.fold_left function_a_deux_argument 0 [1;2;3;4;5] *)
  (* List.fold_left (fun a b -> a+b)  0 [1;2;3;4;5] *)
  
  List.fold_left (fun acc elm ->
		  if peut_manger acc elm then
		    let ce,ci,ra = elm in
		    let ce2,ci2,ra2 = acc in
		    acc = ce2,ci2,ra2 + sqrt(ra)
		  else acc
		 ) c cells
  
(* fait manger toute les cellules *)
let mange_tous ((cells,joueur):monde) : cellule list =
  (* REMPLACER LE 'assert false' PAR VOTRE CODE *)
  assert false

(******************)
(* gestion du jeu *)
(******************)

let perdu ((cells,joueur):monde) : bool =
  (* REMPLACER LE 'assert false' PAR VOTRE CODE *)
  assert false

(* applique un instant de déplacement à tous les participants *)
let un_temps ((cells,vieuxjoueur):monde) (point:coord) : monde =
  (* REMPLACER LE 'assert false' PAR VOTRE CODE *)
  assert false

(**********)
(* dessin *)
(**********)

let draw_cell ((x,y),_,rad) =
  Graphics.set_color Graphics.blue;
  Graphics.fill_circle x y rad

let draw_joueur ((x,y),_,rad) =
  Graphics.set_color (Graphics.red);
  Graphics.draw_circle x y rad

let draw_monde ((cells,joueur):monde) : unit =
  List.iter draw_cell cells;
  draw_joueur joueur

(*****************)
(* initialisation *)
(*****************)

let init_cell rad = (rand_point 800 800),(rand_point 800 800),rad

let init_nb nb : cellule list =
  let rec aux acc nb =
    if nb = 0 then acc
    else aux (init_cell (5 + Random.int 20)::acc) (nb-1)
  in aux [] nb

(********)
(* main *)
(********)

let _ =
  Random.self_init ();
  let open Graphics in
  Graphics.open_graph " 800x800";
  auto_synchronize false;
  Graphics.set_window_title "agar.ml";
  let joueur:cellule = init_cell 20 in
  let world : monde = (init_nb 20),joueur in
  let sleep millis = ignore(Sys.command ("sleep "^(string_of_float millis))) in
  let rec loop (w:monde) dir =
    sleep 0.03;
    synchronize();
    clear_graph();
    let mx,my = mouse_pos() in
    draw_monde w;
    let new_dir = mx,my in
    loop (un_temps w new_dir) new_dir
  in
  loop world (0,0)
