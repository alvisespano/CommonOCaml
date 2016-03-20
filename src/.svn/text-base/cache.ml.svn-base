(*
 * Common library
 * cache.ml: cache type
 *
 * (C) 2007 H-Care srl.
 *)

(** Polymorphic caching facilities.
*)
 
open Prelude 
open Printf
open Cleanup

(** Method dictionary for defining access to the cache, where ['a] is the cache key type and ['b] is the storage type.
The [load] method is called on a cache miss to retrieve an entry.
The [store] method is called on a cache push, flush or writeback to store an entry.
The [size] method is called for querying the size of a given entry.
*)
type ('key, 'data) access = 
  <	load    : 'key -> 'data;    
	store   : 'key -> 'data -> unit;  
	size_of : 'data -> int >

(**/**)
type 'data entry = {
	mutable e_data  		: 'data;
	mutable e_dirty 		: bool;
	mutable e_usage 		: int;
	mutable e_size  	 	: int;
	mutable e_timestamp 	: float;
  }
(**/**)

(** Cache abstract superclass.
Creates a cache of ['b] indexed by the key type ['a] whose maximum size can be limited to [size] (in bytes), whose maximum number of entries can be limited to [entries]
and whose access methods is defined by the [access] dictionary.
*)
class virtual ['key, 'data] t ?size ?entries (access : ('key, 'data) access) =
  object (self) inherit destructible
    (**/**)
 	val total_size = new XThread.mut 0
	val tbl : ('key, 'data entry) Hashtbl.t XThread.sync = new XThread.sync (Hashtbl.create 23)
	val write_sync = new XThread.sync ()  		  			  
    (**/**)

    (** To be implemented depending on cache policy. Must evaluate [Some key] when there is some entry that can be removed in case a cache shrink
    is needed; [None] otherwise. *)
  	method virtual to_be_removed : 'key option

    (** Evaluates [Some data] in case entry [k] exists; [None] otherwise. *)
    method private find k =
 		try
			tbl#apply (fun tbl -> 
	 			let e = Hashtbl.find tbl k
	 			in	 				
					e.e_timestamp <- Time.now ();
	 				Some e)
 		with Not_found -> None	  
 	
 	(** Removes entry [e] associated to key [k] from the cache. *)
  	method private delete k e =
  		tbl#apply (fun tbl -> 
			Hashtbl.remove tbl k;
			total_size#apply_and_set (fun n -> n - e.e_size))
  		
  	(** Inserts entry [e] associated to key [k] into the cache. *)
  	method private insert k e = 
   		tbl#apply (fun tbl -> 
			Hashtbl.add tbl k e;
			total_size#apply_and_set (fun n -> n + e.e_size))

  			
  	method private on_entry f k = on_some f (self#find k)
  	method private forall_keys f = tbl#apply (fun tbl -> Hashtbl.iter (fun k _ -> f k) tbl)
  	method private forall_entries f = tbl#apply (fun tbl -> Hashtbl.iter (fun _ e -> f e) tbl)

	method private shrink = 
		let f a b = match b with 
			None 	-> ()
		  | Some b' -> let rec __f () = on_some (fun k -> if (a ()) > b' then (self#flush k; __f ())) self#to_be_removed 
					   in 
							__f ()
		in
			f (fun () -> self#total_size) size;
			f (fun () -> self#total_entries) entries 
	  
  	 	
  	(* public interface *)

    (** Gets the current total size of the cache in bytes *)
 	method total_size = total_size#get
 	
 	(** Gets the current number of entries stored in the cache *)
	method total_entries = tbl#apply Hashtbl.length

    (** Evaluates either some triple [(size, usage, timestamp)] for the entry [k] or [None] if [k] does not exist. *)
	method stats k = 
	  try		
		tbl#apply (fun tbl -> 
			let x = Hashtbl.find tbl k
			in
				Some (x.e_size, x.e_usage, x.e_timestamp))
	  with Not_found -> None

    (** Invoked automatically on destruction. It forces a full cache flush. *)
  	method destructor = self#flush_all
  	
  	(** Forces a push of entry [k]. *)
  	method force_push k = self#dirty k; self#push k
  	
  	(** Forces the dirty flag of entry [k]. *)
  	method dirty k = self#on_entry (fun e -> e.e_dirty <- true) k
  	
  	(** If entry [k] is dirty forces a store of the associated data. *)
  	method push k = self#on_entry (fun e -> if e.e_dirty then (access#store k e.e_data; e.e_dirty <- false) else ()) k
  	
  	(** Invalidates entry [k], i.e. removes it from the cache. *)
  	method inv k = self#on_entry (fun e -> self#delete k e) k

    (** Pushes and invalidates entry [k]. *)  	
  	method flush k = self#push k; self#inv k
  	
  	(** Forces a push of the whole cache. *)
  	method force_push_all = self#forall_keys self#force_push
  	(** Forces the dirty flag of all cache entries. *)
  	method dirty_all 	  = self#forall_keys self#dirty
  	(** Pushes the whole cache. *)
    method push_all 	  = self#forall_keys self#push
    (** Flushes the whole cache. *)
  	method flush_all 	  = self#forall_keys self#flush
  	(** Invalidates the whole cache. *)
  	method inv_all 	      = self#forall_keys self#inv

    (** Evaluates a list of all cache keys. *)
	method keys = tbl#apply (fun tbl -> Hashtbl.fold (fun k _ acc -> k :: acc) tbl [])

    (** Read entry [k] from the cache. If the read misses, the [load] method of the [access] dictionary is invoked and the entry retrived
    and stored into the cache. *)
  	method read k =
  		match self#find k with
  			Some e -> e.e_usage <- e.e_usage + 1; e.e_data
  		  | None   -> let x = access#load k in
  		  			  let e = { 
						e_data = x; 
						e_dirty = false; 
						e_usage = 1; 
						e_size = access#size_of x; 
						e_timestamp = Time.now () 
 						}
  		  			  in
  		  				  self#insert k e;
						  self#shrink;
  		  				  x
	
	(** Writes a new entry [x] associated to key [k] into the cache. The data is not stored outside the cache until the next writeback.*)
  	method write k x = write_sync#apply (fun () ->
  		let sz = access#size_of x
  		in
	  		(match self#find k with
	  			Some e ->
	  				total_size#apply_and_set (fun n -> n - e.e_size + sz);
  					e.e_data <- x;
  					e.e_dirty <- true;
  					e.e_size <- sz;
					e.e_timestamp <- Time.now ()				
	  		  | None ->
	  		  		self#insert k { 
						e_data = x; 
						e_dirty = true; 
						e_usage = 0; 
						e_size = sz;
						e_timestamp = Time.now ()
					  }); self#shrink)
	  	
	(** Writes a new entry [x] associated to key [k] into the cache and stores it outside immediately as well.*)
 	method writethrough k x = self#write k x; self#push k
  end  
  
(** Cache class with least-used policy.
*)
class ['key, 'data] lu ?size ?entries  (access : ('key, 'data) access) =
  object inherit (['key, 'data] t ?size ?entries access) as super

	method to_be_removed =
  		let f k' e' = function
  			None 			 -> Some (k', e')
  		  | Some (_, e) as z -> if e'.e_usage < e.e_usage then Some (k', e') else z
  		in
  			tbl#apply (fun tbl -> map_some fst (Hashtbl.fold f tbl None))

  end

(** Cache class with least-recently-used policy.
*)
class ['key, 'data] lru ?size ?entries (access : ('key, 'data) access) =
  object inherit (['key, 'data] t ?size ?entries access) as super

	method to_be_removed =
  		let f k' e' = function
  			None 			 -> Some (k', e')
  		  | Some (_, e) as z -> if e'.e_timestamp < e.e_timestamp then Some (k', e') else z
  		in
  			tbl#apply (fun tbl -> map_some fst (Hashtbl.fold f tbl None))

  end
  
  
