
(****************************************)
(*                Dessin                *)
(****************************************)
open Generation
open Graphics


let get_dimensions laby =
  let l = Array.length laby.grid in
  let h = Array.length laby.grid.(0) in
  h,l

let taille_case = 30

(* fonction à utiliser pour mettre à l'échelle *)
let scale (x,y) =
  (x*taille_case , y*taille_case)

(* Fonctions responsables de l'affichage de la torche : *)
let normalize (x,y) size =
  let (x,y) = (float_of_int x),(float_of_int y) in
  let taille_case = float_of_int taille_case in
  let dist = sqrt (x*.x +. y*.y) in
  let x = x /. dist in
  let y = y /. dist in
  let x = x *. taille_case in
  let y = y *. taille_case in
  let s = (float size) in
  int_of_float (x*. s), int_of_float(y *. s)

  let mask (a,b,c) (bl,br,tl,tr) laby =
  let (h,l) = scale (get_dimensions laby) in
  let fill_rect x y sx sy = if sx > 0 && sy > 0 then fill_rect x y sx sy in
  let lx = List.sort (fun (x,y) (xx,yy) -> if x > xx then -1 else 1) [a;b;c] in
  let rightmost= List.hd lx in
  let leftmost = List.hd (List.rev lx) in
  let ly = List.sort (fun (x,y) (xx,yy) -> if y > yy  then -1 else 1) [a;b;c] in
  let upmost = List.hd ly in
  let downmost = List.hd (List.rev ly) in
  let (minx,miny) = bl in
  let (maxx,maxy) = tr in
  fill_rect 0 0 l miny;
  fill_rect 0 maxy l (h-maxy);
  fill_rect 0 0 minx h;
  fill_rect maxx 0 (l-maxx) h;
  set_color black;
  fill_poly [| rightmost ; tr ; upmost |];
  fill_poly [| leftmost ; upmost ; tl |];
  fill_poly [| downmost ; br ; rightmost |];
  fill_poly [| leftmost ; bl ; downmost |];
  set_color black

let bounding_box (a,b,c) =
  let min3 a b c = min (min a b) c in
  let max3 a b c = max (max a b) c in
  let (xa,ya) = a in
  let (xb,yb) = b in
  let (xc,yc) = c in
  let minx = min3 xa xb xc in
  let maxx = max3 xa xb xc in
  let miny = min3 ya yb yc in
  let maxy = max3 ya yb yc in
  let bl = (minx,miny) in
  let br = (maxx,miny) in
  let tl = (minx,maxy) in
  let tr = (maxx,maxy) in
  (bl,br,tl,tr)

let affiche_lampe laby pos =
  let gamer = laby.gamer in
  let (xgamer,ygamer) = scale gamer in
  let (xa,ya as a) = (xgamer+taille_case/2), (ygamer+taille_case/2) in
  let xb,yb = pos in
  let ab = (xb-xa,yb-ya) in
  let bc = (yb-ya,xa-xb) in
  let bcx,bcy = normalize bc 2 in
  let (abx,aby) = normalize ab 5 in
  let (xb,yb as b) = (xa+abx , ya+aby) in
  let (xc,yc as c) = (xb+bcx,yb+bcy) in
  let (xd,yd as d) = (xb-bcx,yb-bcy) in
  set_color yellow;
  draw_poly [|a;c;d|];
  set_color black;
  let box = bounding_box (a,c,d) in
  mask (a,d,c) box laby;
  ()

(* *********** *)
(* TODO *)

let init_graphics l h =
  Graphics.open_graph (" "^(string_of_int l)^"x"^(string_of_int h));
  Graphics.set_window_title "Labyrinthe";
  Graphics.synchronize ()

let affiche_murs c pos =
  let (x,y) = pos in
  if c.s then draw_segments [|(x*taille_case,y*taille_case,x*taille_case+taille_case,y*taille_case)|];
  if c.n then draw_segments [|(x*taille_case,y*taille_case+taille_case,x*taille_case+taille_case,y*taille_case+taille_case)|];
  if c.e then draw_segments [|(x*taille_case+taille_case,y*taille_case,x*taille_case+taille_case,y*taille_case+taille_case)|];
  if c.w then draw_segments [|(x*taille_case,y*taille_case,x*taille_case,y*taille_case+taille_case)|]

let affiche_grid tab =
  for i=0 to (Array.length tab)-1
  do
    for j=0 to (Array.length tab.(0))-1
    do
      affiche_murs tab.(i).(j) (i,j)
    done
  done

let affiche_gamer pos =
  let (x,y) = pos in
  Graphics.set_color Graphics.red;
  Graphics.fill_circle (x*taille_case+taille_case/2) (y*taille_case+taille_case/2) (taille_case-2);
  Graphics.set_color Graphics.black
  
let affiche_coins l =
  Graphics.set_color Graphics.yellow;
  List.iter (fun (x,y) -> Graphics.fill_circle (x*taille_case+taille_case/2) (y*taille_case+taille_case/2) (taille_case-2)
	    ) l;
  Graphics.set_color Graphics.black

let affiche_jokers i =
  Graphics.set_color Graphics.green;
  Graphics.moveto (Graphics.size_x-Graphics.size_x/taille_case) (Graphics.size_y-Graphics.size_y/taille_case);
  Graphics.draw_string (string_of_int i);
  Graphics.set_color Graphics.black

let affiche_laby laby =
  affiche_grid laby.grid;
  affiche_gamer laby.gamer;
  affiche_coins laby.coins;
  affiche_jokers laby.jokers
  
let _main_ =
  init_graphics 600 600;
  while true do () done
