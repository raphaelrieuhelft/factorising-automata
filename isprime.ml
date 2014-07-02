(*implémentation du test de primalité de Miller-Rabin*)

let expmod a b m = 
  let rec aux a b m acc = 
    if b = 0 then acc
      else if b mod 2 = 0 
      then aux ((a * a) mod m) (b/2) m acc
      else aux ((a*a) mod m) ((b-1)/2) m ((a*acc) mod m)
  in
  aux a b m 1

let isprime n k = 
  (* n supposé impair > 3*)
  Random.self_init ();
  let rec makeodd n s =
    if n mod 2 = 0 
    then makeodd (n/2) (s+1)
    else (n,s) 
  in
  let (d,s) = makeodd (n-1) 0 in
  (* n-1 = d*2^s, s maximum, on sait s >= 1*)
  
  let rec test x i = 
    if i = 0 then false
    else let y = (x * x) mod n in 
	 match y with
	 |1 -> false
	 |_ when y = n-1 -> true
	 |_-> test y (i-1)
  in

  let rep = ref true in
  let remtests = ref k in
  while (!rep && (!remtests > 0)) do
    let a =  2 + Random.int (n-3) in
    let x = expmod a d n in
    rep:= x=1 || x=n-1 || test x (s-1);
    decr remtests
  done;
  !rep

let isprime_string s = 
  let n = int_of_string s in
  if  isprime n 100
  then Format.printf "%d is probably prime@." n
  else Format.printf "%d is composite@." n

let () = Arg.parse [] isprime_string ""
