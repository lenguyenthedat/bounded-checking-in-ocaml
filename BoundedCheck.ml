(* Copyright (C) 2010 Le Nguyen The Dat       
   University of Oxford                        
   Computer Science Laboratory *)

(* =========================== import LIST ======================= *)

module List = struct
  include List
  
  let rec take : int -> 'a list -> 'a list =
	fun i -> (fun list -> if i == 0 then [] else ((List.hd list)::(take (i-1) (List.tl list))) )  

  let rec span : ('a -> bool) -> 'a list -> 'a list * 'a list =
    fun p -> function
        [] -> [],[]
      | x::xs when p x ->
          let ys,zs = span p xs in
          (x::ys,zs)
      | xs -> [],xs
(* `Span` divides a list in to 2 list, one contains only elements 
	that pass the function, the other is the original *)

  let rec groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list =
    fun p -> function
        [] -> []
      | x::xs ->
          let ys,zs = span (p x) xs in
          (x::ys) :: groupBy p zs

(* `groupBy` Groups those similar elements together:
	groupBy: list of elements -> list of groups *)

  let group xs = groupBy (=) xs   (* Group those equal elements together *)
end

				
(* =========================== import SHOW ======================= *)
				
type pretty_str = Format.formatter -> unit -> unit

module type PSHOW = sig 		(* pretty show *)
  type t
  val show : t -> pretty_str
end

module type SHOW = sig        (* Normal Show *)
  type t
  val show : t -> string
end

module Show(P:PSHOW) = struct
  open Buffer
  open Format
  type t = P.t
  let show : t -> string =
    fun x ->
      let f _ =
        let str = contents stdbuf in
        clear stdbuf;
        str
      in
      clear stdbuf;
      kfprintf f str_formatter "@[%a@]@?" (P.show x) ()
end

module PShow_list(Elt:PSHOW) = struct
  type t = Elt.t list
  let show : t -> pretty_str =
    fun xs fmt () ->
      let pp = Format.fprintf in
      match List.map Elt.show xs with
          [] -> pp fmt "[]"
        | a1::an ->
            let pprest f =
              List.iter (fun e -> pp f ";@ %a" e ())
            in
	    pp fmt "[%a%a]" a1 () pprest an
end

module PShow_char = struct
  type t = char
  let show : t -> pretty_str =
    fun c fmt () ->
      Format.fprintf fmt "%C" c
end

module PShow_bool = struct
  type t = bool
  let show : t -> pretty_str =
    fun b fmt () ->
      Format.fprintf fmt "%b" b
end


module PShow_int = struct
  type t = int
  let show : t -> pretty_str =
    fun c fmt () ->
      Format.fprintf fmt "%d" c
end




(* =========================== Auxiliary function ======================= *)
	(* for list comprehension *)
	
let rec range i j = if i > j then [] else i :: (range (i+1) j)

let comprehensive2 : 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list =
  fun lst1 lst2 f -> 
	let rec appL l1 l2 = if (List.length l1) <= 0 then []
					     else 
						 (List.append 
							(List.map (f (List.hd l1)) l2) 
							(appL (List.tl l1) l2))
													
	in appL lst1 lst2

let comprehensive3 : 'a list -> 'b list -> 'c list -> ('a -> 'b -> 'c -> 'd) -> 'd list =
  fun lst1 lst2 lst3 f -> 
	comprehensive2 
	   (comprehensive2 lst1 lst2 (fun x y -> (x,y))) 
	   lst3 
	   (fun (x,y) z -> f x y z)

let const : 'a -> 'b -> 'a =
	fun a b -> a

let uncurry : ('a->'b->'c) -> ('a*'b -> 'c) =
	fun f -> (fun (x, y) -> f x y)

let uncurry3 : ('a->'b->'c->'d) -> ( 'a*'b*'c ->'d ) =
	fun f -> (fun (x, y, z) -> f x y z)

let uncurry4 : ('a->'b->'c->'d->'e) -> ( 'a*'b*'c*'d->'e ) =
	fun f -> (fun (x, y, z, t) -> f x y z t)
	
(* =========================== Series ======================= *)

type 'a series = int -> 'a list

(* enumerators functions *)

(* (\/) *)
let sumSeries : 'a series -> 'a series -> 'a series =
	fun s1 s2 -> (fun d -> List.append (s1 d) (s2 d)) 		

(* (><) *)
let productSeries : 'a series -> 'b series -> ('a*'b) series =
 	fun s1 s2 -> (fun d -> (comprehensive2 (s1 d) (s2 d) (fun x y -> (x,y))))
			
 	(* fun s1 s2 -> (fun d -> [ (x,y) | x <- s1 d, y <- s2 d] )			 *)
				


(* ------------------- <methods for type enumeration> ------------------ *)


module type SERIAL = sig
  type t
  val series : t series
  val coseries : 'a series -> (t ->'a) series 
end



		(* ================== Auxiliaries for SERIAL =============== *)

let cons0 : 'a -> 'a series =
	fun c _ -> [c]

module Cons1(T:SERIAL) = struct
  let cons1 : (T.t -> 'a) -> 'a series =
   	fun c -> (fun d -> List.map c (T.series (d-1)) ) 
end

module Cons2(A:SERIAL)(B:SERIAL) = struct
  let cons2 : (A.t -> B.t -> 'a) -> 'a series =
   	fun c d -> List.map (fun (a, b) -> c a b) ((productSeries A.series B.series) (d-1))   
end

module Cons3(A:SERIAL)(B:SERIAL)(C:SERIAL) = struct
  let cons3 : (A.t -> B.t -> C.t -> 'a) -> 'a series =
   	fun c d -> List.map (fun (x, y, z) -> c x y z) 
               ((fun t -> comprehensive3 (A.series t) (B.series t) (C.series t) (fun x y z -> (x,y,z)) ) (d-1))   
end

let alts0 : 'a series -> 'a series = 
	fun ser d -> ser d

		(* ================== ====================== =============== *)



(* same as using instance in haskell *)
module Serial_unit = struct
  type t = unit
  let series      = fun d -> [()]
  let coseries rs d  = List.map (fun b -> (fun () -> b)) (rs d)
  (* let coarbitrary () gen = variant 0 gen *)
end


module Serial_int = struct
  type t = int
  let series      = fun d -> range (-d) d
  let rec coseries rs d = 
	let alts1 = (fun bs d -> if d > 0 then coseries bs (d-1) 
	                        else  List.map (fun x -> (fun _ -> x)) (bs d)) in
		comprehensive3 (alts0 rs d) (alts1 rs d) (alts1 rs d) 
						(fun z f g -> (fun i -> if i > 0 then f (i-1)
						                        else if i < 0 then g (abs i - 1)
												else z )) 
end


module Serial_char = struct
  module S = Serial_int
  type t = char
  let series      = fun d -> List.map Char.chr (List.take (d+1) (range 97 122))
			(* the 'take' function implemented above *)
  let coseries rs d = List.map (fun f -> (fun c -> f (Char.code c - Char.code 'a'))) (S.coseries rs d)
end


module Serial_pair(Fst:SERIAL)(Snd:SERIAL) = struct
  type t = Fst.t * Snd.t
  let series = productSeries Fst.series Snd.series 
  let coseries rs d = List.map (uncurry) (Fst.coseries (Snd.coseries rs) d)
end


module Serial_triple(Fst:SERIAL)(Snd:SERIAL)(Trd:SERIAL) = struct
  type t = Fst.t * Snd.t * Trd.t
  let series = fun d -> comprehensive3 (Fst.series d) (Snd.series d) (Trd.series d) (fun x y z -> (x,y,z))
  (* let series = fun d -> [(a,b,c) 
			| (a,(b,c))<- (productSeries (Fst.series d) (productSeries (Snd.series d) (Trd.series d)))]  *)
  let coseries rs d = List.map (uncurry3) (Fst.coseries (Snd.coseries (Trd.coseries rs)) d)
end


module Serial_bool = struct
  type t = bool
  let series 	  = sumSeries (cons0 true) (cons0 false) 
  (* let series      = fun d -> [true,false] *)
  let coseries rs d = comprehensive2 (rs d) (rs d) (fun r1 r2 -> (fun b -> if b then r1 else r2)) 
end

module Serial_list(Elt:SERIAL) = struct
  type t = Elt.t list
  let rec series d = 
		if d <= 0 then [[]]
		else List.append [[]] (comprehensive2 (Elt.series (d-1)) (series (d-1)) (fun e es -> e::es))
  let rec coseries rs d = let alts2 = (fun cs d -> if d > 0 then (Elt.coseries (coseries cs) (d-1) ) 
                                                  else List.map (fun x -> (fun _ _ -> x)) (cs d)) in
	comprehensive2 (alts0 rs d) (alts2 rs d) 
                        (fun y f -> (fun xs -> match xs with
  											[] -> y
  										   | (x::xs') -> f x xs'))
end

module Serial_func(A:SERIAL)(B:SERIAL) = struct
  type t = (A.t -> B.t)
  let series 	  = A.coseries B.series
  let coseries rs d = 
	let args = A.series d in
	let rec nest arguments i = match arguments with 
		[] -> List.map (fun c -> (fun [] -> c)) (rs d)
	  | (ar::ars) -> List.map (fun f -> (fun (b::bs) -> f b bs)) (B.coseries (nest ars) d)
	in List.map (fun g -> (fun f -> g (List.map (fun a -> f a) args))) (nest args d)	
end

(*********** testable ************)
  
type result = {
  ok : bool option;  (* `option` is the `maybe` version of ocaml *)
  arguments : pretty_str list;
}

let nothing : result = {ok=None; arguments=[]}

type pr = Prop of result list

type property = Property of (int -> pr)

let result : result -> pr =
  fun res -> Prop [res]


 (* -------- *)

module type TESTABLE = sig
  type t
  val property : t -> int -> pr
end

module Testable_bool = struct
  type t = bool
  (* let property b _ = result {nothing with ok=Some b} *)
  let property b _ = Prop [{nothing with ok=Some b}]
  (* let property b _ = Prop [{Some b; []}] *)
end

module Testable_pr = struct
  type t = pr
  let property p _ = p
end

module Testable_property = struct
  type t = property
  let property (Property f) d = f d
end

(* module Testable_fun --- SEE BELOW - after ForALL *)

module Evaluate(T:TESTABLE) = struct
  let evaluate : T.t -> result series =
    fun a d -> 
	  let Prop rs = T.property a d in
      rs
end

module ForAll(S:PSHOW)(T:TESTABLE) = struct
  module E = Evaluate(T)

  let forAll : S.t series -> (S.t -> T.t) -> property =
  	fun xs f -> Property 
		( fun d -> 
		 ( Prop 
		  (List.concat (List.map (fun x -> 
			List.map (fun r -> 
							{r with arguments = S.show x :: r.arguments}) 
			(E.evaluate (f x) d)) 
		   (xs d)) )
		 ) 
		) 
			
		(* [ {r with arguments = S.show x :: r.arguments}  
		| x <- xs d, r <- E.evaluate (f x) d] ) ) *)

  let forAllElem : S.t list -> (S.t -> T.t) -> property =
 	fun xs -> forAll (const xs)

  let unique : 'a list -> bool =
	fun xs -> if List.length xs == 1 then true else false
 
  let notNull : 'a list -> bool =
	fun xs -> if List.length xs == 0 then false else true
  
  let pass : result -> bool =
	fun r -> if r.ok == Some false then false else true
	
  let existence : bool ->  S.t series -> (S.t -> T.t) -> property = 
    fun u xs f -> 
	 let witnesses d = List.map (S.show) 
		( List.filter (fun x -> List.for_all (pass) (E.evaluate (f x) d)) (xs d) ) in
	 let enough = if u then unique else notNull in
	 let valid d = enough (witnesses d) in
	 let argument d = if (valid d) then []
                     else if (notNull (witnesses d)) 
 						  then (* "non-uniqueness" :: *) (List.take 2 (witnesses d)) 
                          else [(* "non-existence" *)] in
     let existenceDepth d = Prop 
			[{ ok = Some (valid d); arguments = argument d}] in
	 Property existenceDepth
  
  let thereExists : S.t series -> (S.t -> T.t) -> property = existence false

  let thereExists1 : S.t series -> (S.t -> T.t) -> property = existence true
  
  let thereExistsElem : S.t list -> (S.t -> T.t) -> property = 
	fun xs -> thereExists (const xs)

  let thereExists1Elem : S.t list -> (S.t -> T.t) -> property =
	fun xs -> thereExists1 (const xs)
end


module Implies(T:TESTABLE) = struct
  let imp : bool -> T.t -> property =
    fun b a ->
      if b
      then Property (T.property a)
      else Property (const (result nothing)) 
   (* else Property (fun d -> (result nothing)) *) 
end


module Testable_fun
  (A:SERIAL)
  (S:PSHOW with type t = A.t)
  (T:TESTABLE) =
struct
  module F = ForAll(S)(T)
  type t = A.t -> T.t
  let property : t -> int -> pr =
    fun f ->
	  let Property f' = F.forAll A.series f
      in f'
end


module Exists(A:SERIAL)(S:PSHOW with type t = A.t)(T:TESTABLE) = struct
  module F = ForAll(S)(T)
  let exists : (A.t -> T.t) -> property = F.thereExists (A.series)
  
  let exists1 : (A.t -> T.t) -> property = F.thereExists1 (A.series)
	
  let existsDeeperBy : (int -> int) -> (A.t -> T.t) -> property = 
	fun f -> F.thereExists (fun d -> A.series (f d)) 
 
  let exists1DeeperBy : (int -> int) -> (A.t -> T.t) -> property =
	fun f -> F.thereExists1 (fun d -> A.series (f d)) 
	
end




(* --------------------------------------------------------------------
-- Testing *)

(* we only do boundedCheck and depthCheck here, where:

in boundedCheck, the user provide the depth d, and all the values from 
depth 0 to d will be checked

in depthCheck, only depth d will be checked

at each depth, enumerate all the possible result by 
  "let Prop results = T.property t dFrom in"
then check those results.
If null -> true
If all true or nothing -> true
any false -> false
 *)

module IterCheck(T:TESTABLE) = struct	
	
  let rec iterCheck : int -> int -> int -> int -> T.t -> result list -> bool -> unit =
	fun ntest nfailCond dFrom dTo t rs flag -> 
		 (* flag true --means-> new depth - hasn't done anything
			flag false --means-> still working on the old depth, keep doing *)

		if (dFrom > dTo) then Format.printf 
		   "\n----------------\n****Finish with no failure****\n\n" 		
		else if (flag == true) then 
								 let () = Format.printf "\nDepth %d:" dFrom in
								 let Prop results = T.property t dFrom in
		 						 iterCheck ntest nfailCond dFrom dTo t results false
			 else 
			 	if (List.length rs == 0) 
				then 
					 let () = Format.printf "\nCompleted %d test(s) without failure. " ntest in
				 	 if (nfailCond > 0) 
					 then 
						 let () = Format.printf "\nBut %d did not meet ==> condition." nfailCond in
					 	 iterCheck 0 0 (dFrom+1) dTo t rs true (* finish this depth *)
				 	 else
						 iterCheck 0 0 (dFrom+1) dTo t rs true (* finish this depth *)

			 	else 
					match (List.hd rs).ok with
			 	        None -> iterCheck (ntest+1) (nfailCond+1) dFrom dTo t (List.tl rs) false
			 										(* only works with *implies* *)
			
			 	   	   |Some true -> iterCheck (ntest+1) (nfailCond) dFrom dTo t (List.tl rs) false
													(* one test passed *)
			
			 	   	   |Some false -> (let p f = function [] -> ()
			 	      	                      			| h::t -> h f ();
			 	      	                         List.iter (fun s -> Format.fprintf f "@ %a" s ()) t
		 	      	                   in Format.printf "@[<2>Falsifiable, after %d tests. Test value:@ %a@]@."
      	      					       	  ntest p (List.hd rs).arguments)
end


module BoundedCheck(T:TESTABLE) = struct
  module I=IterCheck(T)
  let boundedCheck : int -> T.t -> unit =
	fun d t -> I.iterCheck 0 0 0 d t [] true (* check from depth 0 to d *)
  let depthCheck : int -> T.t -> unit =
	fun d t-> I.iterCheck 0 0 d d t [] true (* check only from depth d to d *)
end

