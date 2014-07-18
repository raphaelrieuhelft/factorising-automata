(*On préserve comme invariant que la somme des lignes est juste, on
  déplace les zéros. Une case sur la même ligne et colonne qu'un 1
  doit être un 1 dans une solution : les 0 tâchent d'éviter ces cases*)
open Unix

let n = ref 6 
let p_cheat = ref 1.
let p_ag = ref 0.05

let nrows = ref ((!n/2)+1)
let ncols = ref ((!n)-1)

let time = ref 0
let moves = ref 0
let samplesize = ref 30
let ff = ref Format.std_formatter
let timeout = ref 10000000
exception Conflict

(*rows contient le nombre de 1 dans chque ligne et cols dans chaque
  colonne (diagonale si on tient compte des retenues) *)


let shd = ref 0
let shu = ref 0
let spd = ref 0
let spu = ref 0
let sumd = ref 0
let sumu = ref 0


let try_cheat () = Random.float 1. < !p_cheat
let try_ag () = Random.float 1. < !p_ag

let safego i j (t, rows, cols) (newrow, newcol) = rows.(i)<=(1-newrow)||cols.(j)<=(1-newcol)
let safestay i j (t,rows, cols) = rows.(i)=0||cols.(j)=0

(* add et rm ajoutent ou retirent un ZERO de la case donnée en argument*)


let add i j (t, rows, cols) = match t.(i).(j) with
  |1 -> rows.(i)<-rows.(i)-1; cols.(j)<-cols.(j)-1; t.(i).(j)<-0
  |_ -> Format.printf "la case (%d,%d) contient déjà 0@." i j; raise Conflict 

let rm i j (t, rows, cols) = match t.(i).(j) with
  |0 -> rows.(i)<-1+rows.(i); cols.(j)<-1+cols.(j); t.(i).(j)<-1
  |_ -> Format.printf "la case (%d,%d) contient déjà 1@." i j; raise Conflict



(*SHIFT_DOWN/UP : déplacer un 0 vers le haut ou le bas en l'échangeant
  avec un 1; SUM_UP/DOWN : sommer deux 1 voisins verticalement, mettre
  le résultat en face de celui du haut/bas; SPLIT_UP/DOWN :
  décomposer un 1 en deux 1 sur sa droite et en haut/bas*)


let shift_down i j ((t, rows, cols) as b) = 
  
  if (i<(!nrows-1))&&(j<(!ncols -1))&&(t.(i).(j)=0)&&(t.(i+1).(j+1)=1)
    &&((not (safestay i j b)) || try_ag ())
    &&((safego (i+1) (j+1) b (0,0))||try_cheat())
   
  then (rm i j b; add (i+1) (j+1) b; incr moves; incr shd; true)
  else false

let shift_up i j ((t, rows, cols) as b) =  
  if (i>0)&&(j>0)&&(t.(i).(j)=0)&&(t.(i-1).(j-1)=1) &&((not (safestay i j b)) || try_ag ())
  &&((safego (i-1) (j-1) b (0,0)|| try_cheat ()))
   
  then (rm i j b; add (i-1) (j-1) b; incr moves; incr shu;  true)
  else false

let sum_down i j ((t, rows, cols) as b)= 
  if (i>0)&&(j<(!ncols-1))&&(t.(i).(j)=0)&&(t.(i-1).(j)=1)&&(t.(i).(j+1)=1)&&
    (safego (i-1) (j) b (0,1) ||try_cheat ())&& ((not (safestay i j b)) || try_ag ())&& 
    (safego i (j+1) b (1,0) || try_cheat ())
   
  then (rm i j b; add i (j+1) b; add (i-1) j b; incr moves; incr sumd;
  true)
  else false
  
let split_up i j ((t, rows, cols) as b)=
  if
  (i>0)&&(j<(!ncols-1))&&(t.(i).(j)=1)&&(t.(i-1).(j)=0)&&(t.(i).(j+1)=0)&&((not
  (safestay i (j+1) b && safestay (i-1) j b)) || try_ag ())
    &&(safego i j b (1,1) (*false*)|| try_cheat ())
    
  then (add i j b; rm i (j+1) b; rm (i-1) j b; incr moves; incr spu; true)
  else false

let split_down i j ((t,rows,cols) as b) = 
  if
    (i<(!nrows-1))&&(j<(!ncols-2))&&(t.(i).(j)=1)&&(t.(i).(j+1)=0)&&(t.(i+1).(j+2)=0)&&
      ((not ((safestay i (j+1) b)&&(safestay (i+1) (j+2) b)))|| try_ag ()) &&
      (safego i j b (1,0)|| try_cheat ())
  then (add i j b; rm i (j+1) b; rm (i+1) (j+2) b; incr moves; incr
  spd; true)
  else false

let sum_up i j ((t, rows, cols) as b) =
  if 
    (i<(!nrows-1))&&(j<(!ncols-2))&&(t.(i).(j)=0)&&(t.(i).(j+1)=1)&&(t.(i+1).(j+2)=1)&&
      (safego i (j+1) b (1,0) || try_cheat ())&& (safego (i+1) (j+2) b (0,0) || try_cheat ()) && 
      ((not (safestay i j b)) || try_ag ())
  then (rm i j b; add i (j+1) b; add (i+1) (j+2) b; incr moves; incr
  sumu;  true)
  else false

let update ((t, rows, cols) as b)  =
  let i,j = (Random.int !nrows, Random.int !ncols) in
  match Random.int 6 with
  |0 -> shift_down i j b
  |1 -> shift_up i j b
  |2 -> split_up i j b
  |3 -> split_down i j b
  |4 -> sum_up i j b
  |_ -> sum_down i j b

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
    else (safestay i j b || t.(i).(j)=1)&&(aux i (j+1))
  in
  aux 0 0

let read_result (t, rows, cols) = 
  let f = function 0 -> 0 |_-> 1 in
  (int_of_list (List.map f (List.rev (Array.to_list cols)))), 
  (int_of_list (List.map f  (Array.to_list rows)))

let find_factors target = 
  (*Format.printf "Looking for factors of %d with p_ag = %f, p_cheat = %f@." target !p_ag !p_cheat;*)
  let (t, rows, cols) = init target in
  let fini = ref false in
  while (not !fini) do
    (*if (!time mod 1000000 = 0) then (Unix.sleep 1; print_board t);*)
    (*if !time > !timeout then fini := true;*)
    incr time;
    if (update (t, rows, cols))
	then ((*print_board t; Unix.sleep 3;*)
	  if (check_fini (t, rows, cols))
	  then (fini := true; (* print_board t*))
	)
  done;
  read_result (t,rows, cols)
 
let print_facts target (a,b) = 
  Format.printf "%d = %d * %d@." target a b


let print_time target (a,b) = 
  if !time > !timeout then Format.fprintf !ff "timeout@." else
  Format.fprintf !ff "%d@." !time

let options = ["-ag", Arg.Float (fun f -> p_ag:=f), "sets p_ag"; 
	       "-ch", Arg.Float (fun f -> p_cheat :=f), "sets p_ag";
	      "-s", Arg.Set_int samplesize, "sets sample size";
	      "-timeout", Arg.Set_int timeout, "sets timeout"]

							     
(*let _ = Arg.parse options (fun s -> let n = (int_of_string s) in
	

		       print_facts n (find_factors n)) ""*)


let stats n = 
  for i = 1 to !samplesize do
    print_time n (find_factors n)
  done

let iter nmin nmax = 
  for i = nmin to nmax do
    let fd = Unix.openfile (string_of_int i) [O_WRONLY; O_CREAT] 0o640
    in
    ff:=Format.formatter_of_out_channel (out_channel_of_descr fd);
    stats i
  done

let avg n = 
  let tot = ref 0 in
  for i = 1 to !samplesize do 
    let _ = find_factors n in
    tot := !tot + !time
  done;
  Format.printf "Temps moyen = %d pour n = %d, p_ag = %f@." (!tot /
  !samplesize) n !p_ag;
  let rtot = !shu + !shd + !spu + !spd + !sumu + !sumd in
  Format.printf "Utilisation des règles : 
shift_up = %d%%,
shift_down = %d%%,
split_up = %d%%,
sum_down = %d%%,
split_down = %d%%,
sum_up = %d%%@." (!shu*100/rtot) (!shd*100/rtot) (!spu*100/rtot) (!sumd*100/rtot) (!spd*100/rtot) (!sumu*100/rtot)
(*
let _ = Arg.parse options (fun s -> let n = (int_of_string s) in
				   print_facts n  (find_factors n)
)
 ""
*)

let _ = Arg.parse options (fun s -> avg (int_of_string s)) ""
