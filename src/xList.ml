(*
 * Common library
 * xList.ml: extended list libray
 *
 * (C) 2006 H-Care srl. Alvise Spanò
 *)
 
(** Stdlib List module extension. *) 
 
open Prelude 
 
(** Split a list on the pivot found by predicate [p]. *)
let rec pivotp p = function
	x :: xs -> if p x then ([], x, xs) else let (h, c, t) = pivotp p xs in (x :: h, c, t)
  | _		-> raise Not_found

(** Split list [l] on the pivot [x]. An equality operator [eq] can be optinally passed. *)
let pivot ?(eq = (=)) x l = let (a, _, b) = pivotp (eq x) l in (a, b)

(** Discards elements from list [l] starting from [x]. An equality operator [eq] can be optinally passed. *)
let trim ?(eq = (=)) x l = 
	let rec skip = function [] -> [] | x' :: xs as l -> if eq x x' then skip xs else l
	in
		List.rev (skip (List.rev (skip l)))	

(** Checks whether [x] occurs in [l]. An equality operator [eq] can be optinally passed. *)
let occurs ?(eq = (=)) x l = List.exists (fun x' -> eq x x') l

(** Replaces [x] with [y] in [l]. An equality operator [eq] can be optinally passed. *)
let replace ?(eq = (=)) x y l = List.map (fun x' -> if eq x x' then y else x) l

(** Removes all occurences of [x] in [l]. An equality operator [eq] can be optinally passed. *)
let remove_all ?(eq = (=)) x l = List.filter (fun x' -> not (eq x x')) l

(** Evaluates the difference between [l1] and [l2]. An equality operator [eq] can be optinally passed. *)
let diff ?(eq = (=)) l1 l2 = List.filter (fun x -> not (occurs ~eq x l2)) l1

(** Evaluates the intersection between [l1] and [l2]. An equality operator [eq] can be optinally passed. *)
let intersect ?(eq = (=)) l1 l2 = List.filter (fun x -> occurs ~eq x l2) l1

(** Checks whether [x] occurs in [l] and in case it does removes it. An equality operator [eq] can be optinally passed. *)
let occurs_and_remove ?(eq = (=)) x l = try let (h, t) = pivot ~eq x l in (h @ t, true) with Not_found -> (l, false)

(** Removes the first occurence of [x] in [l]. An equality operator [eq] can be optinally passed. *)
let remove ?(eq = (=)) x l = fst (occurs_and_remove ~eq x l)

(** Checks whether [x] occurs as first element of the list of pair [l]. An equality operator [eq] can be optinally passed. *)
let occurs_fst ?(eq = (=)) x l = List.exists (fun (x', _) -> eq x x') l

(** Checks whether [x] occurs as second element of the list of pair [l]. An equality operator [eq] can be optinally passed. *)
let occurs_snd ?(eq = (=)) x l = List.exists (fun (_, x') -> eq x x') l

(** Checks whether [x] occurs as first element of the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let occurs3_fst ?(eq = (=)) x l = List.exists (fun (x', _, _) -> eq x x') l

(** Checks whether [x] occurs as second element of the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let occurs3_snd ?(eq = (=)) x l = List.exists (fun (_, x', _) -> eq x x') l

(** Checks whether both [x] and [y] occur as first and second element of the list of triples [l]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. *)
let occurs3 ?(eq_fst = (=)) ?(eq_snd = (=)) x y l =	List.exists (fun (x', y', _) -> eq_fst x x' && eq_snd y y') l

(** Returns the second element of the pair whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let assoc_fst ?(eq = (=)) x l = snd (List.find (fun (x', _) -> eq x x') l)

(** Returns the first element of the pair whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let assoc_snd ?(eq = (=)) x l = fst (List.find (fun (_, x') -> eq x x') l)

(** Returns the second and third element of the triple whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let assoc3_fst ?(eq = (=)) x l = match List.find (fun (x', _, _) -> eq x x') l with (_, a, b) -> (a, b)

(** Returns the first and third element of the triple whose second element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let assoc3_snd ?(eq = (=)) x l = match List.find (fun (_, x', _) -> eq x x') l with (a, _, b) -> (a, b)

(** Returns the third element of the triple whose first and second element are [x] and [y] in the list of triples [l]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. *)
let assoc3 ?(eq_fst = (=)) ?(eq_snd = (=)) x y l =
	match List.find (fun (x', y', _) -> eq_fst x x' && eq_snd y y') l with (_, _, z) -> z

(** Returns all third elements of the triples whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let assoc3_fst' ?(eq = (=)) x l =
	let l = List.filter (fun (x', _, _) -> eq x x') l
	in
		List.map (fun (_, _, z) -> z) l
	
(** Returns all second elements of the triples whose first element is [x] in the list of triples [l]. An equality operator [eq] can be optinally passed. *)
let assoc3_snd' ?(eq = (=)) x l =
	let l = List.filter (fun (_, x', _) -> eq x x') l
	in
		List.map (fun (_, _, z) -> z) l

(** Maps the second elements of the pairs whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let map_assoc_fst f ?(eq = (=)) x l = List.map (fun ((x', y) as t) -> if eq x x' then (x, f y) else t) l

(** Maps the first elements of the pairs whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let map_assoc_snd f ?(eq = (=)) x l = List.map (fun ((y, x') as t) -> if eq x x' then (f y, x) else t) l

(** Replaces with [y] all the second elements of the pairs whose first element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let replace_assoc_fst ?(eq = (=)) x y l = List.map (fun ((x', _) as t) -> if eq x x' then (x, y) else t) l

(** Replaces with [y] all the first elements of the pairs whose second element is [x] in the list of pairs [l]. An equality operator [eq] can be optinally passed. *)
let replace_assoc_snd ?(eq = (=)) x y l = List.map (fun ((_, x') as t) -> if eq x x' then (y, x) else t) l

(** Replaces with [f z] all third elements of the triples whose first and second elements are [x] and [y]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. *)
let replace_assoc3 f ?(eq_fst = (=)) ?(eq_snd = (=)) x y l =
	List.map (fun ((x', y', z) as t) -> if eq_fst x x' && eq_snd y y' then (x, y, f z) else t) l

(** Replaces with [z] all the third elements of the triples whose first and second elements are [x] and [y]. An equality operator [eq] can be optinally passed. *)
let replace_assoc3_fst x y z = replace_assoc3 (fun _ -> z) ~eq_snd:(fun _ _ -> true) x y

(** Removes from the list of pairs [l] all pairs whose first element is [x]. An equality operator [eq] can be optinally passed. *)
let remove_assoc_fst ?(eq = (=)) x l = List.filter (fun (x', _) -> not (eq x x')) l

(** Removes from the list of pairs [l] all pairs whose second element is [x]. An equality operator [eq] can be optinally passed. *)
let remove_assoc_snd ?(eq = (=)) x l = List.filter (fun (_, x') -> not (eq x x')) l

(** Removes from the list of triples [l] all triples whose first element is [x]. An equality operator [eq] can be optinally passed. *)
let remove_assoc3_fst ?(eq = (=)) x l = List.filter (fun (x', _, _) -> not (eq x x')) l

(** Removes from the list of triples [l] all triples whose second element is [x]. An equality operator [eq] can be optinally passed. *)
let remove_assoc3_snd ?(eq = (=)) x l = List.filter (fun (_, x', _) -> not (eq x x')) l

(** Removes from the list of triples [l] all triples whose first element is [x] and second element is [y]. Equality operators [eq_fst] and [eq_snd] can be optinally passed. *)
let remove_assoc3 ?(eq_fst = (=)) ?(eq_snd = (=)) x y l =
	List.filter (fun (x', y', _) -> not (eq_fst x x' && eq_snd y y')) l	

(** Returns the second element of the list of pairs [l] given a predicate [p] on the first element. *)
let assocp p l = snd (List.find (fun (x, _) -> p x) l)

(** Filters out all [None] elements and returns a list with all [Some] elements. *)
let filter_some l = List.fold_right (fun el acc -> match el with
		None	-> acc
	  | Some x	-> x :: acc) l []
            
(** Creates a list given a function [f], to which the increasing counter [i] is passed, that returns [None] when for ending the list creation
or [Some x] for storing [x] into the list at the current position.
*)
let tabo f =
	let rec g l i =
		match f i with
			None   -> l
		  | Some x -> g (x :: l) (i + 1)
	in
		List.rev (g [] 0)
        		
(** Creates a list of length [n] given a function [f], to which the increasing counter [i] is passed.
*)     		
let tab f n = tabo (fun i -> if i < n then Some (f i) else None)
		
(** [List.map] variation with index. *)
let mapi f l = List.rev (fst (List.fold_left (fun (l', i) x -> (f i x :: l', i + 1)) ([], 0) l))

(** [List.iter] variation with index. *)
let iteri f l = let i = ref 0 in List.iter (fun x -> f i x; incr i) l

(** [List.fold_left] variation with index. *)
let foldi_left f z l = snd (List.fold_left (fun (i, z) x -> (i + 1, f z x i)) (0, z) l)

(** Remove element [i]-th from list [l]. *)
let remi i l = 
	if (List.length l) <= i then l else
	let rec remi' cur acc l =
		match (cur, l) with
		    (x, _ :: l) when x = i    -> (List.rev acc) @ l
		  | (x, elt :: l) when x <> i -> remi' (x + 1) (elt :: acc) l
		  | _	 					  -> raise (Unexpected "remi: index less than zero?")
	in
		remi' 0 [] l

(** Seek list [l] for [x] and returns its position. *)
let index ?(eq = (=)) x l =
	let rec f i = function
		[] 		 -> raise Not_found
	  | x' :: xs -> if eq x x' then i else f (i + 1) xs
	in
		f 0 l

(** Finds the maximum element of a list given a [compare] function. *)
let find_max compare = function
	[]     -> None
  | [x]    -> Some x
  | h :: t -> Some (List.fold_left (fun x x' -> if compare x x' > 0 then x else x') h t)

(** Finds the minimum element of a list given a [compare] function. *)
let find_min compare = find_max (fun x x' -> -(compare x x'))  

(** Converts a list into a queue. *)
let queue_of_list l =
	let q = Queue.create () in
	let rec f = function
		[]  	-> q
	  | x :: xs -> Queue.push x q; f xs
	in
		f l

(** Converts a queue into a list. *)
let list_of_queue q = Queue.fold (fun l x -> l @ [x]) [] q

(** Extracts the first [n] elements from a list and returns both them and the remainder. *)
let rec heads_with_tail n = function
	[] -> ([], [])
	
  |	x :: xs as l ->
		if n = 0 then ([], l)
		else let (h, t) = heads_with_tail (n - 1) xs in (x :: h, t)
		
(** Extracts the first [n] elements from a list. *)		
let	heads n l = fst (heads_with_tail n l)

(** Extracts the first [n] elements from a list and returns the remainder. *)
let tail n l =
	let len = List.length l
	in
		snd (heads_with_tail (len - (crop (0, len) n)) l)

(** Gets the last element of a list. *)
let rec last = function
	[]     -> raise (Failure "XList.last: empty list")
  |	[x]    -> x
  | _ :: l -> last l
  
(** Creates a Set from a list. *)
module SetOfList (S : Set.S) =
  struct
  	let f = List.fold_left (fun set x -> S.add x set) S.empty
  end

(** Finds an element in list [l] according to the predicate [optp] or, if none is found, returns the best element according to the comparison
function [is_better_than] over the type returned by the evaluation function [eval].
*)
let find_or_best optp eval is_better_than l =
    let rec f (best, bestv) = function
        []      -> `Best (best, bestv)
      | x :: xs -> (match optp x with
                        Some y -> `Found (x, y)
                      | None   -> let v = eval x in f (if is_better_than v bestv then (x, v) else (best, bestv)) xs
                      )
    in
        match l with
            []              -> `Empty
          | best :: _ as l  -> f (best, eval best) l
           
(** Pick the [n]-th element of list [l] (tail recursive). *)
let rec nth l n =
    match l with
        []      -> raise (Failure "XList.nth")
      | x :: xs -> if n = 0 then x else nth xs (n - 1)

(** Randomly picks up an element from a list. Raises [Invalid_argument] if the list is empty. *)
let rnd_nth = function
	[]	-> raise (Unexpected "rnd_nth: empty list")
  | l   -> nth l (rnd_int (0, List.length l - 1))
  
(** Picks up an random element from a list of (float, 'a), according to the weight designated by the first element *)
let weight_assoc l =
	let l = List.filter (fun (x, _) -> x <> 0.) l in
	let (tot, l') = List.fold_left (fun (tot, l) (x, y) -> let tot = tot +. x in (tot, (tot, y) :: l)) (0., []) l
	in
		assocp (fun p -> Random.float tot <= p) (List.rev l')

(** Pick the min element according to the optional compare function *)
let min ?(compare = Pervasives.compare) = function
	[]		-> raise (Failure "XList.min: empty list")
  | x :: l	-> let f acc y = if compare acc y < 0 then acc else y in List.fold_left f x l

(** Pick the max element according to the optional compare function *)
let max ?(compare = Pervasives.compare) = function
	[]		-> raise (Failure "XList.max: empty list")
  | x :: l	-> let f acc y = if compare acc y > 0 then acc else y in List.fold_left f x l	

