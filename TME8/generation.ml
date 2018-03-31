(* le type de notre état de jeu *)
type laby = {
    grid : cell array array;
    coins : pos list;
    gamer : pos;
    jokers : int
  }
and cell = { n : bool ; s : bool ; w : bool ; e : bool }
and pos = int * int

(****************************)
(* Génération de labyrinthe *)
(****************************)

(* tirage d'une permutation aléatoire d'un tableau *)
(* knuth-fisher-yates shuffle *)
let array_shuffle t =
  let n = Array.length t in
  for i = n-1 downto 1 do
    let k = Random.int (i+1) in
    let tmp = t.(i) in
    t.(i) <- t.(k);
    t.(k) <- tmp
  done

(* tirage d'une permutation aléatoire d'une liste *)
let list_shuffle l =
  let t = Array.of_list l in
  array_shuffle t;
  Array.to_list t

(* creation d'un tableau 2D contenant des cellules fermees *)
let create_closed height width =
  Array.make_matrix height width {n = true; s = true; e = true; w = true}

(* tirage d'une case aléatoire du labyrinthe *)
let choose_random_cell grid =
  let height = Array.length grid and
  width = Array.length grid.(0) in
  Random.int height, Random.int width

(* renvoie tous les voisins valides d'une case *)
let get_neighbors (i,j) grid =
  let height = Array.length grid and
  width = Array.length grid.(0) in
  let neighbors = [ (i-1, j) ; (i,j-1); (i,j+1); (i+1, j)] in
  List.filter
    (fun (i,j) -> i >= 0 && i < height && j >= 0 && j < width)
    neighbors

(* retrait des murs entre deux cases adjascentes *)
let rec remove_wall (i,j) (i',j') grid =
  if i < i' then (
    grid.(i).(j) <- {grid.(i).(j) with e = false};
    grid.(i').(j') <- {grid.(i').(j') with w = false}
  )
  else if j < j' then (
    grid.(i).(j) <- {grid.(i).(j) with n = false};
    grid.(i').(j') <- {grid.(i').(j') with s = false}
  )
  else remove_wall (i',j') (i,j) grid

(* parcours du labyrinthe pour en faire un labyrinthe parfais *)
let rec explore current grid visited_cells =
  let neighbors =
    let voisins = get_neighbors current grid in
    let voisins = List.filter (fun (i,j) -> not visited_cells.(i).(j)) voisins in
    list_shuffle voisins
  in
  List.fold_left (fun grid ((i,j) as neighbor)->
    if visited_cells.(i).(j) then grid
    else begin
      visited_cells.(i).(j) <- true;
      remove_wall current neighbor grid;
      explore neighbor grid visited_cells
    end) grid neighbors

(* crée une grille de labyrinthe de taille height * width *)
let generate_labyrinthe height width =
  let grid = create_closed height width in
  let visited = Array.make_matrix height width false in
  let current = choose_random_cell grid in
  explore current grid visited

(* pour rendre le jeu plus facile, on va retirer certains murs au hasard *)
let remove grid ratio =
  let height = Array.length grid
  and width = Array.length grid.(0) in
  let retire = float (height * width) *. ratio |> int_of_float in
  for i = 0 to retire * 2 do
    let c = choose_random_cell grid in
    let neighbor = get_neighbors c grid in
    remove_wall c (List.hd (list_shuffle neighbor)) grid
  done

(* generation des pieces sur la grille *)
let gen_coin grid nb =
  let rec aux acc nb =
  if nb <= 0 then acc
  else
    let ncoin = choose_random_cell grid in
    if List.mem ncoin acc then
      aux acc nb
    else
      aux (ncoin :: acc) (nb - 1)
  in aux [] nb

(* generation de labyrinthe :
      - generation de la grille
      - on retire 50% des murs
      - generation des pieces
      - position aléatoire pour le joueur *)
let generate_laby l h nb_piles=
  let grid = generate_labyrinthe l h in
  remove grid 0.5;
  let coins = gen_coin grid 10 in
  let gamer = choose_random_cell grid in
  {grid; coins; gamer; jokers  = nb_piles}
