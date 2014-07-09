(*On préserve comme invariant que la somme des lignes est juste, on
  déplace les zéros. Une case sur la même ligne et colonne qu'un 1
  doit être un 1 dans une solution : les 0 tâchent d'éviter ces cases*)
open Unix

let n = ref 7 
let p_cheat = ref 1.
let p_ag = ref 0.05

let nrows = ref 3
let ncols = ref 4

let time = ref 0
let moves = ref 0
let samplesize = ref 30
let ff = ref Format.std_formatter
let timeout = ref 10000000
exception Conflict

(*rows contient le nombre de 1 dans chque ligne et cols dans chaque
  colonne (diagonale si on tient compte des retenues) *)



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

let corner i j = ((i,j)=(0,0))||((i,j)=(0,!ncols-1))
  ||((i,j)=(!nrows-1, 0))||((i,j)=(!nrows-1,!ncols-1))
  (*en fait (0, ncols-1) inutile : conserver la somme implique
    conserver la parité*)

let shift_down i j ((t, rows, cols) as b) = 
  
  if (i<(!nrows-1))&&(j<(!ncols -1))&&(not ((corner i j)||(corner
    (i+1) (j+1)))) && (t.(i).(j)=0)&&(t.(i+1).(j+1)=1)
    &&((not (safestay i j b)) || try_ag ())
    &&((safego (i+1) (j+1) b (0,0))||try_cheat())
   
  then (rm i j b; add (i+1) (j+1) b; incr moves; true)
  else false

let shift_up i j ((t, rows, cols) as b) =  
  if (i>0)&&(j>0)&&(not ((corner i j)||(corner (i-1) (j-1))))&&(t.(i).(j)=0)&&(t.(i-1).(j-1)=1) &&((not (safestay i j b)) || try_ag ())
  &&((safego (i-1) (j-1) b (0,0)|| try_cheat ()))
   
  then (rm i j b; add (i-1) (j-1) b; incr moves; true)
  else false

let sum_down i j ((t, rows, cols) as b)= 
  if (i>0)&&(j<(!ncols-1))
    &&(not ((corner i j)||(corner (i-1) j)||(corner i (j+1)))) 
    && (t.(i).(j)=0)&&(t.(i-1).(j)=1)&&(t.(i).(j+1)=1)&&
    (safego (i-1) (j) b (0,1) ||try_cheat ())&& ((not (safestay i j b)) || try_ag ())&& 
    (safego i (j+1) b (1,0) || try_cheat ())
   
  then (rm i j b; add i (j+1) b; add (i-1) j b; incr moves; true)
  else false
  
let split_up i j ((t, rows, cols) as b)=
  if
  (i>0)&&(j<(!ncols-1))
    &&(not ((corner i j)||(corner (i-1) j)||(corner i (j+1)))) 
    &&(t.(i).(j)=1)&&(t.(i-1).(j)=0)&&(t.(i).(j+1)=0)&&((not
  (safestay i (j+1) b && safestay (i-1) j b)) || try_ag ())
    &&(safego i j b (1,1) (*false*)|| try_cheat ())
    
  then (add i j b; rm i (j+1) b; rm (i-1) j b; incr moves; true)
  else false

let split_down i j ((t,rows,cols) as b) = 
  if
    (i<(!nrows-1))&&(j<(!nrows-2))
    &&(not ((corner i j)||(corner i (j+1))||(corner (i+1) (j+2)))) 
    &&(t.(i).(j)=1)&&(t.(i).(j+1)=0)&&(t.(i+1).(j+2)=0)&&
      ((not ((safestay i (j+1) b)&&(safestay (i+1) (j+2) b)))|| try_ag ()) &&
      (safego i j b (1,0)|| try_cheat ())
  then (add i j b; rm i (j+1) b; rm (i+1) (j+2) b; incr moves; true)
  else false

let sum_up i j ((t, rows, cols) as b) =
  if 
    (i<(!nrows-1))&&(j<(!nrows-2))
    &&(not ((corner i j)||(corner i (j+1))||(corner (i+1) (j+2))))
    &&(t.(i).(j)=0)&&(t.(i).(j+1)=1)&&(t.(i+1).(j+2)=1)&&
      (safego i (j+1) b (1,0) || try_cheat ())&& (safego (i+1) (j+2) b (0,0) || try_cheat ()) && 
      ((not (safestay i j b)) || try_ag ())
  then (rm i j b; add i (j+1) b; add (i+1) (j+2) b; incr moves; true)
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



(*entiers représentés par des listes de 0/1 : bit de poids faible en premier*)
 
let rec list_of_int a  =
  match a with 
  |0 -> []
  |a -> (a mod 2) :: (list_of_int (a/2))
 

let rec int_of_list l =    
  match l with 
  |[] -> 0
  |h::t -> h + (2*(int_of_list t))

(*soustrait 2^k à un nombre > 2^k représenté en base 2 dans un tableau, bit
  de poids faible en premier*)


let sub2 a k = 
  if a.(k)=1 then a.(k)<-0
  else begin 
    let i = ref k in
    while (a.(!i)=0) do
      a.(!i)<-1;
      incr i
    done;
    a.(!i)<-0
  end
(*reqs : rows > 2, cols >2*)
    
let init target rows cols = (* target : nombre à factoriser*)
  Random.self_init ();
  let a = Array.of_list (list_of_int target) in
  let s = Array.length a in
  ncols:= cols;
  nrows := rows;
  let t = Array.make_matrix !nrows !ncols 0 in
  let trows = Array.make !nrows 0 in
  let tcols = Array.make !ncols 0 in
  if s < rows + cols - 1 then failwith "longueurs trop grandes"
  else begin
    (try (
    sub2 a (rows-1);
    sub2 a (cols-1);
    sub2 a (rows+cols-2)
     ) with _-> failwith "longueurs trop grandes");
    t.(0).(0)<-1;
    t.(rows-1).(0)<-1;
    t.(rows-1).(cols-1)<-1;
    if s < rows + cols + 1 
 (*   then begin  
      for i = 0 to (s-1) do
	if a.(i)=1
	then begin
	  if i < cols-1
	  then t.(1).(cols-(i+1))<-1
	  else t.(i-(cols-2)).(1)<-1
	end
      done
    end
    else if s = rows + cols *)
    then begin
      if s = rows + cols && a.(s-1)=1 then (a.(s-1)<-0; a.(s-2)<-2+a.(s-2));
      let i = ref 0 and j = ref 0 in
      for k = rows + cols - 2 downto 0 do
	if k < cols-1 then (i:=0; j:=cols-(k+1))
	else (i:=k-(cols-1); j:=0);
	while (a.(k)>0&&(!i < rows)&&(!j <  cols)) do
	  if t.(!i).(!j)=0 then (t.(!i).(!j)<-1; a.(k)<-a.(k)-1);
	  incr i;
	  incr j
	done;
	
	if a.(k)>0 then begin
	  if k = 0 then ( failwith "longueurs trop courtes")
	  else (a.(k-1)<-a.(k-1)+(2*a.(k)); a.(k)<-0)
	end
      done
    end
    else failwith "longueurs vraiment trop courtes"
  end;
  for i = 0 to rows-1 do
    for j = 0 to cols-1 do
      if t.(i).(j)=1
      then begin
	trows.(i)<-1+trows.(i);
	tcols.(j)<-1+tcols.(j);
      end
    done
  done;
  time :=0;
  moves:=0;
  t, trows, tcols


    
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

let find_factors target rows cols  = 
 (* Format.printf "Looking for factors of %d with p_ag = %f, p_cheat =
 %f@." target !p_ag !p_cheat;*)
  if target mod 2 = 0 then (2, target/2)
  else if target mod 3 = 0 then (3, target/3)
  else 
  let (t, rows, cols) = init target rows cols in
  let fini = ref false in
  while (not !fini) do
    if (!time mod 100000 = 0) then (Unix.sleep 1; print_board t);
    if !time > !timeout then fini := true;
    incr time;
    if (update (t, rows, cols))
	then ((*print_board t; Unix.sleep 3;*)
	  if (check_fini (t, rows, cols))
	  then (fini := true;  print_board t)
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


let stats n rows cols = 
  for i = 1 to !samplesize do
    print_time n (find_factors n rows cols)
  done
(*
let iter nmin nmax = 
  for i = nmin to nmax do
    let fd = Unix.openfile (string_of_int i) [O_WRONLY; O_CREAT] 0o640
    in
    ff:=Format.formatter_of_out_channel (out_channel_of_descr fd);
    stats i
  done*)


let _ = Arg.parse options (fun _ -> ()) "";
  Scanf.scanf  "%d %d %d" (fun n r c -> print_facts n (find_factors n r c))

