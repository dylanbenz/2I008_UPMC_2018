let swap tab i j =
  let temp = tab.(i) in
  tab.(i)<-tab.(j); tab.(j)<-temp

let check tab x =
  let res = ref false in
  for i=0 to (Array.length tab)-1
  do
    if tab.(i)=x
    then (res:=true; ())
    else ()
  done;!res

let check2 tab x =
  Array.exists (fun e -> e=x) tab

exception Found of int
let findIndex tab v =
try
  ( for i=0 to (Array.length tab)-1
    do
      if tab.(i)=x || tab.(i) < x
      then raise(Found(i));
      else ()
    done;
    raise Not_Found()
  )
with
    Found(u)->u

let findList tab v =
  let l = Array.length tab in
  let rec loop res i=
    if i<l
    then
      if tab.(i)=v
      then i::(loop res (i+1))
      else loop res (i+1)
    else res
  in
  loop [] 0

let array_copy_k_first tab res k =
for i=0 to (k-1)
do
  res.(i)<-tab.(i)
done

let array_shift tab i j =
  for k=j downto i
  do
      tab.(k+1)<-tab.(k)
  done





let rec derivPoly polynome =
  let n = List.length polynome in
  let res = ref [] in
  #TESTER si la liste est vide ? ici
  for i=(n-1) downto 0
  do
    res:=res@[derivMono poynome[i]]
  done; !res


List.map (fun x -> derivMono x) polynome
