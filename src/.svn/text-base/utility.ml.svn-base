(*
 * Common library
 * utility.ml: misc stuff
 *
 * (C) 2005 Quendi srl.
 *)

(** Misc unclassifiable stuff. *)
open Prelude 
open Printf
open Cleanup

  
(* Superset functor given a total order on the element type.
The created class allows to 
*) 
module Superset (T : Set.OrderedType) =
  struct
  	module S = Set.Make (T)
  	
	class t ?super set =
	  object (self)
	  	val mutable set = set
	  	val mutable supero = super
	  	val mutable wsubs = []
	  	
	  	method private register_sub sub = 
	  		let fin _ = wsubs <- List.filter (fun w -> Weak.check w 0) wsubs in
	  		let wsub = Weak.create 1
	  		in
	  			Gc.finalise fin sub;
	  			Weak.set wsub 0 (Some sub);
	  			wsubs <- wsub :: wsubs
	  	
	  	method add x =
	  		set <- S.add x set;
	  		on_some (fun super -> super#add x) supero
	  		
	  	method remove x =
	  		set <- S.remove x set;
	  		List.iter (fun w -> on_some (fun sub -> sub#remove x) (Weak.get w 0)) wsubs
	  		
	  	method is_empty = S.is_empty set
	  	
	  	method clear = S.iter self#remove set

	  	method empty_subset =
	  		let sub = new t ~super:(self :> t) S.empty
	  		in
	  			self#register_sub sub;
	  			sub
	  	
	  	method subset p =
	  		let set' = S.filter p set in
	  		let sub = new t ~super:(self :> t) set'
	  		in
	  			self#register_sub sub;
	  			sub
	  	
	  	method same_subset = self#subset (fun _ -> true)
	  	
	  	method iter f = S.iter f set
	  	method fold f z = S.fold f z set
	  	method exists p = S.exists p set
	  	method elements = S.elements set
	  	method for_all p = S.for_all p set
	  	method cardinal = S.cardinal set
	  end
		
	  
	let empty = new t S.empty
	
	let of_set set = new t set
	
	let of_list l =
		let set = List.fold_left (fun set x -> S.add x set) S.empty l
		in
			new t set
	   
	let of_array a =
		let set = Array.fold_left (fun set x -> S.add x set) S.empty a
		in
			new t set
			 
  end
  

(** Event chain class for registering closures and evaluating them on explicit notification.
*)
class ['ev] event_chain (logger : #Log.logger) =
  object
  	val l : ('ev * (unit -> unit) * int) list XThread.sync = new XThread.sync []
  	
  	(** Notifies event [ev] and trigger hook execution associated to it. *)
  	method notify ev =
  		l#apply (List.iter (fun (ev', hook, _) ->
  			if ev = ev' then
  				try hook ()
  				with Exit -> ()
  				  |  e    -> logger#error ~code:[1;6;1] (sprintf "event_chain: exception trapped: %s" (pretty_exn e))
  			))
  	
  	(** Registers event [ev] with hook [hook] at priority [pri]. *)
  	method add_listener ?(pri = 0) ev hook =
  		l#apply_and_set (fun l -> List.merge (fun (_, _, pri) (_, _, pri') -> -(compare pri pri')) l [(ev, hook, pri)])
  end

(** Class expiration for data with a limited time validity
*)
class ['a] expiration ~decay ~(fetch : unit -> 'a) =
  object
	val mutable content : (float * 'a) option = None
	method get : 'a =
		let f () = let x = fetch () in content <- Some (Time.now (), x); x
		in		
			match content with 
				None 		-> f ()
			  | Some (t, x)	-> if ((Time.now ()) -. t) < decay 
								 then x
							 	 else f () 
  end

