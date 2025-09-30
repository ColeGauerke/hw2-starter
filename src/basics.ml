let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  let (a,b) = tup in (b,a)

let rev_triple (tup : 'a * 'b * 'c) =
  let (a,b,c) = tup in (c,b,a)

let is_odd x = 
  if x mod 2 <> 0 then true else false

let is_older (date1: int * int * int) (date2: int * int * int) =
  date1 < date2

let to_us_format (date1: int * int * int) = 
  let (a,b,c) = date1 in (b,c,a)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************) 

let rec pow x p = 
  if p = 0 then 1 else x * pow x (p-1)

let rec fac n = 
  if n = 0 then 1
  else if n = 1 then 1 else  n * fac(n-1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  List.nth lst idx

let larger lst1 lst2 = 
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 = len2 then []
  else if len1 > len2 then lst1
  else lst2

let sum lst1 lst2 =  
  let combined = lst1 @ lst2 in
  List.fold_left (+) 0 combined