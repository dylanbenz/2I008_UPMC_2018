open Generation
open Affichage
(*open Graphics = eviter d'ecrire Graphics. a chaque appel *)
       
(*********************************************)
(*                GESTION DU JEU             *)
(*********************************************)

type direction = North | South | East | West
let deplace_nord (x,y) = (x, y+1)
let deplace_sud  (x,y) = (x, y-1)
let deplace_est  (x,y) = (x+1, y)
let deplace_west (x,y) = (x-1, y)

(* TODO *)


let event_loop laby =
  let st = Graphics.wait_next_event [Graphics.Key_pressed] in
  match st.Graphics.key with
  | '
  
  affiche_laby laby;
  event_loop laby

let est_legal laby pos1 pos2 =
  
  
let () = ()
