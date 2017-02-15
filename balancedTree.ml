(* Copyright (C) 2010 Le Nguyen The Dat       
   University of Oxford                        
   Computer Science Laboratory *)


open BoundedCheck


type avlTree = 
	| Nil
    | Node of avlTree * int * avlTree

let rec flatten : avlTree -> int list = 
	fun tree -> match tree with 
	                | Nil -> []
	                | Node (l,k,r) ->  (flatten l) @ [k] @ (flatten r)

let rec depthTree : avlTree -> int =
	fun tree -> match tree with
	                | Nil -> 0
	                | Node (l,k,r) -> if (depthTree l >= depthTree r) then depthTree l + 1
                                                                      else depthTree r + 1
	
let rec balanced : avlTree -> bool =
	fun tree -> match tree with 
	                | Nil -> true
	                | Node (l,k,r) -> (balanced l) && (balanced r) && (abs((depthTree l) - (depthTree r)) <= 1) 
	

let leftBalance : avlTree -> int -> avlTree -> avlTree =
	fun tree x r -> match tree with
	                | Node (ll,k, Nil) -> Node (ll,k,Node(Nil,x,r)) 
	                | Node (ll,k, Node (lr1, m, lr2)) -> 
		if depthTree ll > depthTree (Node (lr1, m, lr2))
			then Node (ll, k, (Node ((Node (lr1, m, lr2)), x, r)))
			else Node ((Node (ll, k, lr1)), m, (Node (lr2, x, r)))

let rightBalance : avlTree -> int -> avlTree -> avlTree =
	fun l x tree -> match tree with
	                | Node (Nil,k, rr) -> Node ((Node (l, x, Nil)), k, rr ) 
	                | Node ((Node (rl1, m, rl2)), k, rr) -> 
		if depthTree rr > depthTree (Node (rl1, m, rl2))
			then Node ((Node (l, x, (Node (rl1, m, rl2)))), k, rr)
			else Node ((Node (l, x, rl1)), m, (Node (rl2, k, rr)))
			
let balanceTree : avlTree -> avlTree =
	fun tree -> match tree with
 		  			| Nil -> Nil
                    | Node (l, k, r) -> let weight = (depthTree r) - (depthTree l) in
					if weight < (-1) then leftBalance l k r
					else if weight > 1 then rightBalance l k r
					else Node (l, k, r)

let rec rMost : avlTree -> int =
	 fun tree -> match tree with
					| Node (l, k, Nil) -> k
					| Node (l, k, r) -> rMost r

let rec deleteTree : int -> avlTree -> avlTree =
	fun x tree -> match tree with
	                  | Nil -> Nil
					  | Node (l, k, Nil) ->  if x = k then l else balanceTree (Node ((deleteTree x l), k, Nil))
					  | Node (l, k, r) -> if x = k then balanceTree (Node ((deleteTree (rMost l) l), (rMost l), r))
					 					  else if x > k then balanceTree (Node (l, k, (deleteTree x r)))
									  	  else balanceTree (Node ((deleteTree x l), k, r))

let rec insertTree : int -> avlTree -> avlTree =
		fun x tree -> match tree with 
						| Nil -> Node (Nil, x, Nil)
 						| Node (l, k, r) -> if x = k then Node (l, k, r)
								            else if x > k then balanceTree (Node (l, k, (insertTree x r)))
											else balanceTree (Node ((insertTree x l), k, r))	
											
								         (* else if x > k then  (Node (l, k, (insertTree x r)))
											else then  (Node ((insertTree x l), k, r)) *)
											

(* module rec Serial_tree = struct
  module C = Cons3(Serial_tree)(Serial_int)(Serial_tree) (* recursive module needed!!! *)
  type t = avlTree
  let rec series      = sumSeries (cons0 Nil) (C.cons3 Node)
end *)

module Serial_tree = struct
  module S = Serial_int
  type t = avlTree
  let rec series d = 
	if d <= 0 then [Nil]
	else List.append [Nil] (comprehensive3 (series (d-1)) (S.series (d-1)) (series (d-1)) (fun x y z -> Node (x,y,z)))
  let coseries rs d = [(fun Nil -> List.hd (rs d))]
end

module PShow_tree = struct
  module PS = PShow_list(PShow_int)
  type t = avlTree
  let show : t -> pretty_str =
    fun tree ->
      PS.show (flatten tree)
end

module PShow_pairIntTree = struct
  module Pt = PShow_tree
  type t = (int*avlTree)
  let show : t -> pretty_str =
    fun (i,tree) ->
      Pt.show tree
end


module Testable_tree =
  Testable_fun
    (Serial_pair(Serial_int)(Serial_tree))
    (PShow_pairIntTree)
    (Testable_property) ;;

module C = BoundedCheck(Testable_tree)

module Insert(T:TESTABLE with type t = bool) = struct
module I = Implies(T)
let prop_insert : (int*avlTree) -> property =
  fun (i,tree) -> I.imp (balanced tree) (balanced (insertTree i tree))
let check = C.boundedCheck 4 prop_insert
end

module D = Insert(Testable_bool)
let () = D.check


				
