open BoundedCheck

(* -------------integer--------------------- *)

(* let prop_trivialInt : int -> bool =
  fun i -> (i < 115 )

module Testable_int_to_bool =
  Testable_fun
    (Serial_int)
    (PShow_int)
    (Testable_bool) ;;
module C = BoundedCheck(Testable_int_to_bool)
let () = C.depthCheck 120 prop_trivialInt *)


(* -------------list of char --------------------- *)

(* let prop_revrev : char list -> bool =
  fun xs -> List.rev (List.rev xs) = xs

module Testable_list_to_bool =
  Testable_fun
    (Serial_list(Serial_char))
    (PShow_list(PShow_char))
    (Testable_bool) ;;
module C = BoundedCheck(Testable_list_to_bool)
let () = C.boundedCheck 5 prop_revrev *)

(* -------------list of int --------------------- *)

(* let prop_revrev : int list -> bool =
  fun xs -> List.rev (List.rev xs) = xs

module Testable_list_to_bool =
  Testable_fun
    (Serial_list(Serial_int))
    (PShow_list(PShow_int))
    (Testable_bool) ;;
module C = BoundedCheck(Testable_list_to_bool)
let () = C.boundedCheck 5 prop_revrev *)

(* -------------pair (int & bool) --------------------- *)

(* let prop_revrev : int*bool -> bool =
  fun (i,b) -> (i != 25 or b = true )

module PShow_pairIntBool = struct
  type t = int*bool
  let show : t -> pretty_str =
    fun (a,b) fmt () ->
      Format.fprintf fmt "%d %b" a b
end

module Testable_pair_to_bool =
  Testable_fun
    (Serial_pair(Serial_int)(Serial_bool))
    (PShow_pairIntBool)
    (Testable_bool) ;;
module C = BoundedCheck(Testable_pair_to_bool)
let () = C.boundedCheck 30 prop_revrev *)


(* -------------working with *implies* --------------------- *)

(* module Testable_int_to_prop =
  Testable_fun
    (Serial_int)
    (PShow_int)
    (Testable_property) ;;

module C = BoundedCheck(Testable_int_to_prop)

module Simple(T:TESTABLE with type t = bool) = struct
module I = Implies(T)
let prop_simple : int -> property =
  fun i -> I.imp (i < 0) (i + 5 > 0)
let check = C.boundedCheck 20 prop_simple
end

module D = Simple(Testable_bool)
let () = D.check *)


(* -------------working with higher order function --------------------- *)


(* module PShow_function(Elt:PSHOW) = struct
  type t = (int->char)*(Elt.t list)*(int)
  let show : t -> pretty_str =
    fun (f,ls,i) fmt () ->
      	let pp = Format.fprintf in
	      match List.map Elt.show ls with
	          [] -> pp fmt "[]"
	        | a1::an ->
	            let pprest f =
	              List.iter (fun e -> pp f ";@ %a" e ())
	            in
		    pp fmt "[%a%a]" a1 () pprest an
end

let prop_Map : (int->char)*(int list)*(int) -> bool =
	fun (f,xs,i) -> 
		if (i > List.length xs or i < 1) then true 
		else (List.nth (List.map f xs) (i-1)) = f (List.nth xs (i-1))
	
module Testable_fun_to_bool =
  Testable_fun
	(Serial_triple(Serial_func(Serial_int)(Serial_char))(Serial_list(Serial_int))(Serial_int))
    (PShow_function(PShow_int))
    (Testable_bool) ;;

module C = BoundedCheck(Testable_fun_to_bool)
let () = C.boundedCheck 2 prop_Map *)




(* -------------working with *exists* --------------------- *)

(* module Testable_Int_to_prop =
  Testable_fun
    (Serial_int)
    (PShow_int)
    (Testable_property) ;;
	
module C = BoundedCheck(Testable_Int_to_prop)

module Simple(T:TESTABLE with type t = bool) = struct
module E = Exists(Serial_int)(PShow_int)(T)
let prop_simple : int -> property =
 fun x -> E.existsDeeperBy (fun d -> (d+1)) (fun z -> x < z)
let check = C.boundedCheck 20 prop_simple
end

module D = Simple(Testable_bool)
let () = D.check *)


(* --------QuickSort----------- *)


let rec quicksort : int list -> int list =
 fun xs -> match xs with 
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (fun t -> t < x) xs in
      (quicksort ys) @ (x :: (quicksort zs))

let rec sorted : int list -> bool =
 fun xs -> match xs with
     | [] -> true
     | [x] -> true
     | (x::xs) -> (x <= List.hd xs) && sorted xs

let prop_sort : int list -> bool =
  fun xs -> sorted ( quicksort xs)

module Testable_intlist_to_bool =
  Testable_fun
    (Serial_list(Serial_int))
    (PShow_list(PShow_int))
    (Testable_bool) ;;
module C = BoundedCheck(Testable_intlist_to_bool)
let () = C.boundedCheck 6 prop_sort


(* --------Stable Sorting Algorithm----------- *)

(* let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
    List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let key : int*char -> int = 
 fun (i,c) -> i 

let eq : int*char -> int*char -> bool =
 fun (i,c) (i2, c2) -> (i = i2) && (c = c2)

let adv : int -> int =
 fun i -> if i < 0 then i else i+1

let rec position : int*char -> (int*char) list -> int =
 fun x list -> match list with
  | [] -> -1
  | l::ls -> if (eq x l) then 0 else adv (position x ls) 

let rec quicksort : (int*char) list -> (int*char) list =
 fun xs -> match xs with 
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (fun t -> key t < key x) xs in
      (quicksort ys) @ (x :: (quicksort zs))

let rec stablySorted : (int*char) list -> (int*char) list -> bool =
 fun xs ys -> match ys with
     | [] -> true
     | [y] -> true
     | (head::tail) -> (stablySorted xs tail) && 
						 ((key head < key (List.hd tail))
						  or 
						 ((key head = key (List.hd tail)) && 
						 (position head xs <= position (List.hd tail) xs)))



let prop_stablesort : (int*char) list -> bool =
  fun xs -> stablySorted (uniq xs) (quicksort (uniq xs))

module PShow_pairIntChar = struct
  type t = int*char
  let show : t -> pretty_str =
    fun (a,b) fmt () ->
      Format.fprintf fmt "%d %c" a b
end

module Testable_intlist_to_bool =
  Testable_fun
    (Serial_list(Serial_pair(Serial_int)(Serial_char)))
    (PShow_list(PShow_pairIntChar))
    (Testable_bool) ;;
module C = BoundedCheck(Testable_intlist_to_bool)
let () = C.boundedCheck 5 prop_stablesort *)


(* ----Selection sort - unstable --- *)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
    List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
    Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let key : int*char -> int = 
 fun (i,c) -> i 

let eq : int*char -> int*char -> bool =
 fun (i,c) (i2, c2) -> (i = i2) && (c = c2)

let adv : int -> int =
 fun i -> if i < 0 then i else i+1

let rec position : int*char -> (int*char) list -> int =
 fun x list -> match list with
  | [] -> -1
  | l::ls -> if (eq x l) then 0 else adv (position x ls) 

let rec selectionSort : (int*char) list -> (int*char) list =
 fun xs -> match xs with 
  | [] -> []
  | x::xs -> let rec selectR small output list = match list with
                  | [] -> (small :: (selectionSort output))
	              | l::ls -> if (key l < key small) then selectR l (small::output) ls
	                        else selectR small (l::output) ls
	         in selectR x [] xs

let rec stablySorted : (int*char) list -> (int*char) list -> bool =
 fun xs ys -> match ys with
     | [] -> true
     | [y] -> true
     | (head::tail) -> (stablySorted xs tail) && 
						 ((key head < key (List.hd tail))
						  or 
						 ((key head = key (List.hd tail)) && 
						 (position head xs <= position (List.hd tail) xs)))



let prop_stablesort : (int*char) list -> bool =
  fun xs -> stablySorted (uniq xs) (selectionSort (uniq xs))

module PShow_pairIntChar = struct
  type t = int*char
  let show : t -> pretty_str =
    fun (a,b) fmt () ->
      Format.fprintf fmt "%d %c" a b
end

module Testable_intlist_to_bool =
  Testable_fun
    (Serial_list(Serial_pair(Serial_int)(Serial_char)))
    (PShow_list(PShow_pairIntChar))
    (Testable_bool) ;;
module C = BoundedCheck(Testable_intlist_to_bool)
let () = C.depthCheck 4 prop_stablesort



	
