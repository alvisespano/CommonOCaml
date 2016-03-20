(*
 * Common library
 * timer.ml: timers
 *
 * (C) 2007 Alvise Spano' @ H-Care srl.
 *)

(** Timer handlers. *)

open Prelude
open XThread
open Printf
open XList

(** The type for timer handler IDs. *)
type t = int

(** The type for timer behaviour. *)
type behaviour = [ `Once | `Reload ]

(**/**)
type data = { id         : t;
              expiration : float;
              name       : string;
              spawn      : (unit -> unit) -> unit;
              interval   : float;
              behaviour  : behaviour;
              hook       : t -> unit;
            }
(**/**)

(** Database of all timers. *)	
let all_timers : data list XThread.sync = new XThread.sync []

(** Timestamp of next programmed sigalrm. *)
let next = new XThread.mut 0.0
	
(**/**)
let __dispatcher_resolution = 1.0
let __dispatcher_epsylon = 0.05

type dispatcher_event = Reschedule | Exec

let __dispatcher logger =
    let reschedule () =
        (* all_timers#apply_and_set (List.sort (fun t t' -> compare t.expiration t'.expiration)); *)
		match all_timers#apply ident with
			[]	 	-> 
			  begin
				next#set (Time.now () +. __dispatcher_resolution);
				Thread.delay __dispatcher_resolution;
				Reschedule
			  end
		  | t :: _	-> 
		  begin			
	        let d = t.expiration -. (Time.now ())
	        in
				if d <= __dispatcher_epsylon then Exec
	            else if d > __dispatcher_epsylon && d < __dispatcher_resolution then 
				  begin
					next#set t.expiration;
					Thread.delay d;
					Exec
				  end
				else (* d > __dispatcher_resolution *)
				  begin
					next#set (Time.now () +. __dispatcher_resolution);
					Thread.delay __dispatcher_resolution;
					Reschedule
				  end
		  end
    in			
        while true do 
		  match reschedule () with
            Exec ->
			  begin
				match all_timers#apply ident with
					[]		-> ()
								
							(* potrei avere che nel frattempo il timer in scadenza è stato rimosso *)
				  | t :: _ when t.expiration <= Time.now () -> 
				 	  begin
	                    let logger = LogLib.prefixer logger (sprintf "%s[%d]" t.name t.id) in
	                    (try t.spawn (fun () -> t.hook t.id)
	                    with e -> logger#warn ~code:[1;3;1] (sprintf "exception caught: %s" (pretty_exn e)));
	                    (match t.behaviour with
	                        `Reload -> all_timers#apply_and_set (fun ts ->
								let l = List.map (
									fun t' -> 
										if t.id = t'.id then { t with expiration = t.expiration +. t.interval } 										else t') ts
								in
									List.sort (fun t t' -> compare t.expiration t'.expiration) l)
	                      | `Once   -> all_timers#apply_and_set (List.filter (fun t' -> t.id <> t'.id)))
					  end
				  | _	-> ()                 
			  end
		  | Reschedule -> ()
        done    
            
let __dispatcher_is_alive = new XThread.sync false
(**/**)
	   
(** Adds a new timer handler.
[logger] is a logger.
[name] optionally sets a name for the handler.
[spawn] is the spawn function for the hook - the default is simply the evaluation of the passed closures, which means that the timer dispatcher
itself will evaluate it.
[interval] is the timer interval in seconds.
[since] is the first start time - the default is [interval] seconds from now.
[beh] is the behaviour - [`Reload] means that the timer will be redispatched forver every [interval] seconds starting from [since];
[`Once] means that it will be dispatched only once at [since], thus [interval] has no meaning.
[hook] is the handler function to which the timer hander will be passed.
*)	               		
let add_handler logger ?name ?(spawn = fun f -> f ()) ~interval ?(since = Time.now () +. interval) beh hook =
	let tid = fresh () in
	let name = something (string_of_int tid) name
	in
		all_timers#apply_and_set (fun ts -> 
			let t =  { 
				expiration = since; 
				id = tid; 
				name = name; 
				spawn = spawn; 
				interval = interval; 
				behaviour = beh; 
				hook = hook 
			}
			in
				List.sort (fun t t' -> compare t.expiration t'.expiration) (t :: ts));
		__dispatcher_is_alive#apply_and_set (function
			true	-> true
		  | false 	-> ignore (XThread.create logger "timerdispatcher" __dispatcher logger); true);
	    tid

(** Removes the given timer handler.
*)
let remove_handler tid = all_timers#apply_and_set (List.filter (fun t -> t.id <> tid))
			
(** Modifies the given timer handler with new parameters.
[interval] is the timer interval in seconds.
[since] s the first start time - the default is [interval] seconds from now.
[beh] is the behaviour - [`Reload] means that the timer will be redispatched forver every [interval] seconds starting from [since];
[`Once] means that it will be dispatched only once at [since], thus [interval] has no meaning.
[hook] is the handler function to which the timer hander will be passed.
*)			
let change_handler tid ~interval ?(since = Time.now () +. interval) beh hook = 
	all_timers#apply_and_set (fun ts ->
		let l = List.map (
			fun t -> 
				if tid = t.id then { t with expiration = since; interval = interval; behaviour = beh; hook = hook } 					else t) ts
		in
			List.sort (fun t t' -> compare t.expiration t'.expiration) l)
		
(** Simple class that tracks the creation time and offers a method for checking whether the given [interval] has expired. *)		
class timeout logger ~interval =
  object
    val start = Time.now ()
    method expired = Time.now () > start +. interval
  end    




