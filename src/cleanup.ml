(*
 * Common library
 * cleanup.ml: cleanup and destructors
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Cleanup utilities for applications *) 
 
open Prelude 
open Printf
     
(* cleanup hooks and finalizers *)
(**/**)
let __cleanup_hooks = new XThread.sync []
(**/**)

(** Closures passed as argument will be saved and invoked at program exit. *)
let at_cleanup f = __cleanup_hooks#apply_and_set (fun self -> f :: self)

(**/**)
let __at_weak_cleanup f wa =
	at_cleanup (fun () -> match Weak.get wa 0 with
								None -> ()
		  					  | Some x -> f x)
(**/**)

(** [Gc.finalise] replacement that registers the finalisation function as a cleanup hook, i.e. the finalisation function will be invoked at
exit in any case.
*)		  					  
let finalise f x =
	Gc.finalise f x;
	let wa = Weak.create 1
	in
		Weak.set wa 0 (Some x); 
		__at_weak_cleanup f wa

(** Force a cleanup, i.e. a full GC major cycle plus the invokation of all cleanup hooks.
*)
let cleanup logger =
	Gc.full_major ();
	__cleanup_hooks#apply (List.iter (fun f ->
		try f ()
		with Exit -> ()
		  |  e -> logger#error (sprintf "cleanup: exception trapped: %s" (pretty_exn e))))

		
(**/**)
let __destroy self = self#destructor

(** Superclass for simulating the object destructor paradigm. As [self] is being collected, or at cleanup, the [destructor] method will be invoked.
*)
class virtual destructible =
  object (self)
  	initializer finalise __destroy self
  	method virtual destructor : unit
  end
