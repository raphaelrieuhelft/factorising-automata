(*On préserve comme invariant que la somme des lignes est juste, on
  déplace les zéros. Une case sur la même ligne et colonne qu'un 1
  doit être un 1 dans une solution : les 0 tâchent d'éviter ces cases*)

let n = ref 6 
let p_cheat = ref 0.1
let p_ag = ref 0.02

let nrows = ref ((!n/2)+1)
let ncols = ref ((!n)-1)

let time = ref 0
let moves = ref 0



exception Conflict

(*rows contient le nombre de 1 dans chque ligne et cols dans chaque
  colonne (diagonale si on tient compte des retenues) *)



let try_cheat () = Random.float 1. < !p_cheat
let try_ag () = Random.float 1. < !p_ag

let safe i j (t, rows, cols) = rows.(i)=0||cols.(j)=0

(* add et rm ajoutent ou retirent un ZERO de la case donnée en argument*)


let add i j (t, rows, cols) = match t.(i).(j) with
  |1 -> rows.(i)<-rows.(i)-1; cols.(j)<-cols.(j)-1; t.(i).(j)<-0
  |_ -> Format.printf "la case (%d,%d) contient déjà 0@." i j; raise Conflict 

let rm i j (t, rows, cols) = match t.(i).(j) with
  |0 -> rows.(i)<-1+rows.(i); cols.(j)<-1+cols.(j); t.(i).(j)<-1
  |_ -> Format.printf "la case (%d,%d) contient déjà 1@." i j; raise Conflict



(*SHIFT_DOWN/UP : déplacer un 0 vers le haut ou le bas en l'échangeant
  avec un 1; SUM : sommer deux 1 voisins verticalement; SPLIT :
  changer un 1 en deux 1 sur sa droite*)


let shift_down i j ((t, rows, cols) as b) = 
  
  if (i<(!nrows-1))&&(j<(!ncols -1))&&(t.(i).(j)=0)&&(t.(i+1).(j+1)=1)
    &&((not (safe i j b)) || try_ag ())
    &&((safe (i+1) (j+1) b)||try_cheat())
   
  then (rm i j b; add (i+1) (j+1) b; incr moves; true)
  else false

let shift_up i j ((t, rows, cols) as b) =  
  if (i>0)&&(j>0)&&(t.(i).(j)=0)&&(t.(i-1).(j-1)=1) &&((not (safe i j b)) || try_ag ())
  &&((safe (i-1) (j-1) b || try_cheat ()))
   
  then (rm i j b; add (i-1) (j-1) b; incr moves; true)
  else false

let sum i j ((t, rows, cols) as b)= 
  if (i>0)&&(j<(!ncols-1))&&(t.(i).(j)=0)&&(t.(i-1).(j)=1)&&(t.(i).(j+1)=1)&&
    (safe (i-1) (j) b ||try_cheat ())&& ((not (safe i j b)) || try_ag ())&& (safe i (j+1) b|| try_cheat ())
   
  then (rm i j b; add i (j+1) b; add (i-1) j b; incr moves; true)
  else false
  
let split i j ((t, rows, cols) as b)=
  if
  (i>0)&&(j<(!ncols-1))&&(t.(i).(j)=1)&&(t.(i-1).(j)=0)&&(t.(i).(j+1)=0)&&((not
  (safe i (j+1) b && safe (i-1) j b)) || try_ag ())
    &&(safe i j b|| try_cheat ())
    
  then (add i j b; rm i (j+1) b; rm (i-1) j b; incr moves; true)
  else false

let update ((t, rows, cols) as b)  =
  let i,j = (Random.int !nrows, Random.int !ncols) in
  match Random.int 6 with
  |0 -> shift_down i j b
  |1 -> shift_up i j b
  |3 -> split i j b
  |_ -> sum i j b
    

(*entiers représentés par des listes de 0/1 : bit de poids faible en premier*)

    
let rec list_of_int a  =
  match a with 
  |0 -> []
  |a -> (a mod 2) :: (list_of_int (a/2))
 

let rec int_of_list l =    
  match l with 
  |[] -> 0
  |h::t -> h + (2*(int_of_list t))

    
    
let init target = (* target : nombre à factoriser*)
  Random.self_init ();
  let a = Array.of_list (list_of_int target) in
  let s = Array.length a in
  ncols:= s - 1;
  nrows := (s / 2)+1;
  let t = Array.make_matrix !nrows !ncols 0 in
  let rows = Array.make !nrows 0 in
  let cols = Array.make !ncols 0 in
  for k = 0 to s-2 do
    if a.(k)=1 
    then (
      let j = !ncols-(1+k) in
      t.(0).(j) <- 1;
      cols.(j)<-1;
      rows.(0)<-1+rows.(0)
    )
  done;
  if a.(s-1) = 1 
  then (
    let j = !ncols+1-s in
    t.(1).(j) <- 1;
    cols.(j) <- 1+cols.(j);
    rows.(1)<-1;
  );
  time :=0;
  moves:=0;
  t, rows, cols

let print_spaces k =
  for i = 1 to k do 
    Format.printf ". "
  done

let print_board t  = 
  Format.printf "Time %d, moves %d : @." !time !moves; 
  for i = 0 to !nrows-1 do
    let kgauche = !nrows-(1+i) in
    let kdroite = !nrows-(1+kgauche) in
    print_spaces kgauche;
    for j = 0 to !ncols-1 do
      Format.printf "%d " t.(i).(j)
    done;
    print_spaces kdroite;
    Format.printf"@."
  done;
  Format.printf "\n@."
    
let check_fini ((t, rows, cols) as b) =
  let rec aux i j = 
    if i >= !nrows then true
    else if j >= !ncols then aux (i+1) 0
    else (safe i j b || t.(i).(j)=1)&&(aux i (j+1))
  in
  aux 0 0
      

let find_factors target = 
  let (t, rows, cols) = init target in
  let fini = ref false in
  while (not !fini) do
    if (!time mod 10000000 = 0) then (Unix.sleep 1; print_board t);
    incr time;
    if (update (t, rows, cols))
	then (
	  if (check_fini (t, rows, cols))
	  then (fini := true;  print_board t)
	)
  done
 
							     
let () = find_factors 561
