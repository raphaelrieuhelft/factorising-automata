(*On préserve comme invariant que la somme des lignes est juste, on
  déplace les zéros. Une case sur la même ligne et colonne qu'un 1
  doit être un 1 dans une solution : les 0 tâchent d'éviter ces cases*)
open Unix

let n = ref 7 
let p_cheat = ref 1.
let p_ag = ref 0.01

let nrows = ref 3
let ncols = ref 4

let time = ref 0
let moves = ref 0

let sigs_only = ref false

let samplesize = ref 30
let ff = ref Format.std_formatter
let timeout = ref 100000000
exception Conflict

let ag_count = ref 0
let ag_ok = ref 5

(*rows contient le nombre de 1 dans chque ligne et cols dans chaque
  colonne (diagonale si on tient compte des retenues) *)
module Cset = Set.Make (struct type t = (int*int) let compare = compare end)

let dir (i1,j1) (i2,j2) = 
  match (i1-i2,j1-j2) with
  |(n,0) when n > 0 ->0
  |(0,n) when n > 0 ->1
  |(n,0) when n < 0 ->2
  |(0,n) when n < 0 ->3
  |_->(-1)

let get_neighbors i j = 
  List.fold_left 
    (fun s (a,b) -> if (i+a>=0 && i+a <(!nrows) && j+b>=0 &&j+b<(!ncols))
      then Cset.add  (i+a,j+b) s else s) Cset.empty 
    [(1,0); (0,1); (-1,0); (0,-1)]
    

let try_cheat () = Random.float 1. < !p_cheat
let try_ag () = Random.float 1. < !p_ag

(*let try_ag () = if Random.float 1. < !p_ag then
    begin
      (*Format.printf "try_ag, count = %d, time = %d@." !ag_count !time;*)
      if !ag_count >= (!ag_ok) 
      then true 
      else (incr ag_count; false) 
    end
  else false
*)
let forall t f =
  let rec aux i =
    if i = Array.length t then true
      else (f t i)&&(aux (i+1))
  in
  aux 0

let safego i j (t, sigs) (i2,j2) = 
  let d = dir (i2,j2) (i,j) in
  forall sigs.(i2).(j2) (fun t i -> i=d || not t.(i))

let safestay i j (t,sigs) = (not (sigs.(i).(j).(0) ||
				    sigs.(i).(j).(2)))|| 
  (not (sigs.(i).(j).(1) || sigs.(i).(j).(3)))

(* add et rm ajoutent ou retirent un ZERO de la case donnée en argument*)


let add i j (t, sigs) = match t.(i).(j) with
  |1 -> t.(i).(j)<-0
  |_ -> Format.printf "la case (%d,%d) contient déjà 0@." i j; raise Conflict 

let rm i j (t, sigs) = match t.(i).(j) with
  |0 -> t.(i).(j)<-1
  |_ -> Format.printf "la case (%d,%d) contient déjà 1@." i j; raise Conflict

(*SHIFT_DOWN/UP : déplacer un 0 vers le haut ou le bas en l'échangeant
  avec un 1; SUM_UP/DOWN : sommer deux 1 voisins verticalement, mettre
  le résultat en face de celui du haut/bas; SPLIT_UP/DOWN :
  décomposer un 1 en deux 1 sur sa droite et en haut/bas*)


(*rand_diag : choisit un élément de même poids que i j différent de
  lui (même colonne en comptant les retenues) *)


let corner i j = ((i,j)=(0,0))||((i,j)=(0,!ncols-1))
  ||((i,j)=(!nrows-1, 0))||((i,j)=(!nrows-1,!ncols-1))
  (*en fait (0, ncols-1) inutile : conserver la somme implique
    conserver la parité*)

let in_bounds i j =
  i>=0&&i<(!nrows)&&j>=0&&j<(!ncols)

let rand_diag i j =
  if corner i j then (i,j) (*pas grave : la règle échouera*)
  else
  let ofsmin = - (min i j) in (*inclus*)
  let ofsmax = min (!nrows - i) (!ncols - j) in (*exclu*)
  try (
  let ofs = if ofsmax-ofsmin=1 then 0 else (Random.int (ofsmax - (1+ ofsmin))) + ofsmin in
  let ofs = if ofs = 0 then ofsmax-1 else ofs in

  assert (in_bounds (i+ofs) (j+ofs));
  (i+ofs, j+ofs)
  ) with _ -> (
     Format.printf "failed with i=%d,j=%d, ofsmin=%d, ofsmax=%d@." i j ofsmin ofsmax; 
     failwith "fail" )
	
(*rand_next_diag : choisit un élément sur la colonne de droite de i j
  (en comptant les retenues) *)

let rand_next_diag i j = 
  let ofs = (min i (j+1)) +1 in
  assert (not (corner i j));
  try
  rand_diag (i-ofs) (j+1-ofs)
  with _-> (
    Format.printf "failed next_diag with i=%d, j=%d@." i j; failwith "fail2")
      
(*

let shift i j ((t,sigs) as b) =
  let (i1,j1) = rand_diag i j in
  if (not ((corner i j)||(corner i1 j1)))&&t.(i).(j)=0&&(t.(i1).(j1)=1)
  &&((not (safestay i j b)) || try_ag ())
  && ((safego i j b (i1,j1)) || try_cheat ())
  then (rm i j b; add i1 j1 b; incr moves; true)
  else false
    
let sum i j ((t,sigs) as b) = 
  let (i1,j1) = rand_next_diag i j in
  let (i2,j2) = rand_diag i1 j1 in
  if (not ((corner i j)||(corner i1 j1)||(corner i2 j2)))&&(t.(i).(j)=0)
    && (t.(i1).(j1)=1)&&(t.(i2).(j2)=1)
    && ((not (safestay i j b))||try_ag ())
    && ((safego i j b (i1,j1) && safego i j b (i2,j2))||try_cheat ())
  then (rm i j b; add i1 j1 b; add i2 j2 b; incr moves; true)
  else false

let split i j ((t,sigs) as b) = 
  let (i1,j1) = rand_next_diag i j in
  let (i2,j2) = rand_diag i1 j1 in
  if (not ((corner i j)||(corner i1 j1)||(corner i2 j2)))&&(t.(i).(j)=1)
    && (t.(i1).(j1)=0)&&(t.(i2).(j2)=0)
    && ((not ((safestay i1 j1 b)&&(safestay i2 j2 b)))||try_ag())
    &&((safego i1 j1 b (i,j) && safego i2 j2 b (i,j))||try_cheat ())
  then (add i j b; rm i1 j1 b; rm i2 j2 b; incr moves; true)
  else false
*)

let shift_down i j ((t, sigs) as b) = 
  
  if (i<(!nrows-1))&&(j<(!ncols -1))&&(not ((corner i j)||(corner
    (i+1) (j+1)))) && (t.(i).(j)=0)&&(t.(i+1).(j+1)=1)
    &&((not (safestay i j b)) || try_ag ())
    &&((safego (i+1) (j+1) b (0,0))||try_cheat())
   
  then (rm i j b; add (i+1) (j+1) b; incr moves; true)
  else false

let shift_up i j ((t, sigs) as b) =  
  if (i>0)&&(j>0)&&(not ((corner i j)||(corner (i-1) (j-1))))&&(t.(i).(j)=0)&&(t.(i-1).(j-1)=1) &&((not (safestay i j b)) || try_ag ())
  &&((safego (i-1) (j-1) b (0,0)|| try_cheat ()))
   
  then (rm i j b; add (i-1) (j-1) b; incr moves; true)
  else false




let sum_down i j ((t, sigs) as b)= 
  if (i>0)&&(j<(!ncols-1))
    &&(not ((corner i j)||(corner (i-1) j)||(corner i (j+1)))) 
    && (t.(i).(j)=0)&&(t.(i-1).(j)=1)&&(t.(i).(j+1)=1)&&
    (safego (i-1) (j) b (0,1) ||try_cheat ())&& ((not (safestay i j b)) || try_ag ())&& 
    (safego i (j+1) b (1,0) || try_cheat ())
   
  then (rm i j b; add i (j+1) b; add (i-1) j b; incr moves; true)
  else false
  
let split_up i j ((t, sigs) as b)=
  if
  (i>0)&&(j<(!ncols-1))
    &&(not ((corner i j)||(corner (i-1) j)||(corner i (j+1)))) 
    &&(t.(i).(j)=1)&&(t.(i-1).(j)=0)&&(t.(i).(j+1)=0)&&((not
  (safestay i (j+1) b && safestay (i-1) j b)) || try_ag ())
    &&(safego i j b (1,1) (*false*)|| try_cheat ())
    
  then (add i j b; rm i (j+1) b; rm (i-1) j b; incr moves; true)
  else false

let split_down i j ((t, sigs) as b) = 
  if
    (i<(!nrows-1))&&(j<(!ncols-2))
    &&(not ((corner i j)||(corner i (j+1))||(corner (i+1) (j+2)))) 
    &&(t.(i).(j)=1)&&(t.(i).(j+1)=0)&&(t.(i+1).(j+2)=0)&&
      ((not ((safestay i (j+1) b)&&(safestay (i+1) (j+2) b)))|| try_ag ()) &&
      (safego i j b (1,0)|| try_cheat ())
  then (add i j b; rm i (j+1) b; rm (i+1) (j+2) b; incr moves; true)
  else false

let sum_up i j ((t, sigs) as b) =
  if 
    (i<(!nrows-1))&&(j<(!ncols-2))
    &&(not ((corner i j)||(corner i (j+1))||(corner (i+1) (j+2))))
    &&(t.(i).(j)=0)&&(t.(i).(j+1)=1)&&(t.(i+1).(j+2)=1)&&
      (safego i (j+1) b (1,0) || try_cheat ())&& (safego (i+1) (j+2) b (0,0) || try_cheat ()) && 
      ((not (safestay i j b)) || try_ag ())
  then (rm i j b; add i (j+1) b; add (i+1) (j+2) b; incr moves; true)
  else false


  


let update ((t, sigs) as b)  =
  let update_sig (i,j) (i1,j1) =
    let d = dir (i,j) (i1,j1) in
    sigs.(i).(j).(d)<-(sigs.(i1).(j1).(d)||t.(i1).(j1)>0)
  in
    
  let i,j = (Random.int !nrows, Random.int !ncols) in
  let s = get_neighbors i j in
  Cset.iter (update_sig (i,j)) s;
 (* if !sigs_only then false else*)
  if corner i j then false else
   (* match Random.int 3 with
    |0 -> shift i j b
    |1 -> split i j b
    |_ -> sum i j b*)
 match Random.int 6 with
  |0 -> shift_down i j b
  |1 -> shift_up i j b
  |2 -> split_up i j b
  |3 -> split_down i j b
  |4 -> sum_up i j b
  |_ -> sum_down i j b
  (*if aux () then (ag_count:=0; true) else false*)

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

let print_safe (t,sigs) =
  Format.printf "Time %d, moves %d : @." !time !moves; 
  for i = 0 to !nrows-1 do
    let kgauche = !nrows-(1+i) in
    let kdroite = !nrows-(1+kgauche) in
    print_spaces kgauche;
    for j = 0 to !ncols-1 do
      Format.printf "%c " (if safestay i j (t, sigs) then 'o' else 'X')
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
 (* let trows = Array.make !nrows 0 in
  let tcols = Array.make !ncols 0 in*)
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

  let sigs = Array.make_matrix rows cols [||] in
  for i = 0 to rows-1 do
    for j = 0 to cols-1 do
      sigs.(i).(j)<-Array.make 4 false
    done
  done;
  time :=0;
  moves:=0;
  sigs_only:=false;
  t, sigs


    
(*let check_fini ((t, rows, cols) as b) =
  let rec aux i j = 
    if i >= !nrows then true
    else if j >= !ncols then aux (i+1) 0
    else (safestay i j b || t.(i).(j)=1)&&(aux i (j+1))
  in
  aux 0 0
*)

let check_fini t =
  let z = Array.make !ncols 0 in
  forall t (fun t i -> t.(i)=t.(0)||t.(i)=z)


let read_result t = 
  let col = Array.make !nrows 0 in
  for i = 0 to !nrows-1 do
    col.(i)<-t.(i).(0)
  done;
  (int_of_list(List.rev (Array.to_list t.(0)))), 
  (int_of_list  (Array.to_list col))


let copy_matrix m =
  let l = Array.length m in
    if l = 0 then m else
      let result = Array.make l m.(0) in
        for i = 0 to l - 1 do
           result.(i) <- Array.copy m.(i)
        done;
        result


let find_factors target rows cols  = 
 (* Format.printf "Looking for factors of %d with p_ag = %f, p_cheat =
 %f@." target !p_ag !p_cheat;*)
  if target mod 2 = 0 then (2, target/2)
  else if target mod 3 = 0 then (3, target/3) 
  else if Isprime.isprime target 100 then (1, target)
  else 
  let (t, sigs) = init target rows cols in
  let fini = ref false in
  if (check_fini t)
  then (fini := true(*;  print_board t*));
  while (not !fini) do
    if (!time mod 1000000 = 0) then (Unix.sleep 1; print_board t(*;
				     print_safe (t,sigs) *));
    (*if !time > !timeout then fini := true;*)
    incr time;
    if (update (t, sigs))
	then ((*print_board t; Unix.sleep 3;*)
	  if (check_fini t)
	  then (fini := true; (*sigs_only:=true ;*) print_board t(*;
						      print_safe
						      (t,sigs) *))
	)
  done;
(*  let u = copy_matrix t in
  let pf = ref true in
  while true do
    (*if (!time mod 1000000 = 0) then (Unix.sleep 1; print_board t;
				      print_safe (t,sigs));*)
    incr time;
    if not !pf 
    then begin
      if (update (t,sigs))&&(forall t (fun t i -> t.(i)=u.(i)))
      then (pf:=true; Format.printf "retour à t = %d@." !time) end
    else
      if (update (t,sigs))&& not (forall t (fun t i -> t.(i)=u.(i)))
      then (pf := false; Format.printf "départ à t = %d@." !time)
  done;*)
  read_result t
 
let print_facts target (a,b) = 
  if a = 1 
  then
    Format.printf "%d is prime@." target
  else
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
  let fd = Unix.openfile ((string_of_int n)^","^(string_of_int
  rows)^","^(string_of_int cols)) [O_WRONLY; O_CREAT] 0o640 in
  ff:=Format.formatter_of_out_channel (out_channel_of_descr fd);
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

(*
let _ = Arg.parse options (fun _ -> ()) "";
  Scanf.scanf  "%d %d %d" (fun n r c -> print_facts n (find_factors n r c))
*)


let avg n r c = 
  let tot = ref 0 in
  for i = 1 to !samplesize do 
    let _ = find_factors n r c in
    tot := !tot + !time
  done;
  Format.printf "Temps moyen = %d pour n = %d, p_ag = %f@." (!tot /
  !samplesize) n !p_ag

let _ = Arg.parse options (fun _ -> ()) "";
  Scanf.scanf "%d %d %d" find_factors
