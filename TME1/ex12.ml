open Graphics

let dessine_segment (x,y) (z,k) =
  Graphics.moveto x y;
  Graphics.lineto z k

let dessine_triangle a b c =
  dessine_segment a b;
  dessine_segment b c;
  dessine_segment c a

let milieu a b =
  let xa,ya = a in
  let xb,yb = b in
  ((xb+xa)/2,(yb+ya)/2)

let rec dessine_sierpinski a b c n =
  for i=0 to 10000 do
    true
  done;
  (if n=0 then dessine_triangle a b c
  else
    (*begin*)
      let ac = milieu a c in
      let ab = milieu a b in
      let bc = milieu b c in
      dessine_sierpinski a ac ab (n-1);
      dessine_sierpinski bc b ab (n-1);
      dessine_sierpinski bc ac c (n-1)
    (*end*)
    )
      
let () =
  (* initialisation de la fenetre graphique *)
  Graphics.open_graph " 800x800";
  Graphics.set_window_title "Sierpinski";
  Graphics.loop_at_exit [] (fun _ -> ());
  (* on construit le triangle initial *)
  let a = (50,50) in
  let b = (400,750) in
  let c = (750, 50) in
  (* on le dessine *)
  let nb_iterations = 10 in
  dessine_sierpinski a b c nb_iterations
