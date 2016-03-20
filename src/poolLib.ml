(*
 * Common library
 * poolLib.ml: typical pools
 *
 * (C) 2007 Alvise Spanò at H-Care srl.
 *)

(** High-level library for Pool. *)

open Prelude
open Printf
open Comm

(** Pool specialization for threads. It offers a commodity method for quick spawning of pooled threads with closures.
*)
class thread_pool ~name ?max ?(inactivity_timeout = 60.) ?inactivity_check_interval ?pre_spawns ?step logger =
  object (self) inherit [XThread.box] Pool.t logger ~name ?max ~inactivity_timeout ?inactivity_check_interval ?pre_spawns ?step
                    ~create:(fun () -> new XThread.box ~name logger)
                    ~finalise:(fun b -> b#terminate)
                    
    (** Spanws a thread from the pool and make it evaluate function [f] passing [x] to it. *)
    method spawn : 'a. string -> ('a -> unit) -> 'a -> unit = fun thname f x ->
		let box = self#acquire
        in
            box#exec thname (fun () -> 
				(try f x 
				with e -> logger#error ~code:[1;2;1] (sprintf "exception caught: %s; releasing thread to pool" (pretty_exn e))); 
				self#release box) ()

(*
        manage_resource 
			(fun () -> logger#debug "before acquiring"; self#acquire) 
			(fun box -> logger#debug "before exec"; box#exec thname (fun () -> f x; logger#debug Log.Normal "after exec"; self#release box) ()) 
			self#release

*)

  end
        

(** Creates a pair of pool subclasses respectively for active connections and passive connections, given a Connector.
The [active] pool will create an new active connection each time a new resource has to be generated.
The [passive] pool will wait for a new passive connection each time a new resource has to be generated.
*)
module ConnectorPool (Conn : Connector) =
  struct     
    class active ~name ?max ?(inactivity_timeout = 300.) ?inactivity_check_interval ?pre_spawns ?step ~active_arg ~disconnect_arg logger =
      object inherit [Conn.channel] Pool.t logger ~name ?max ~inactivity_timeout ?inactivity_check_interval ?pre_spawns ?step
                        ~create:(fun () -> Conn.active_connect logger active_arg)
                        ~finalise:(fun ch -> Conn.disconnect logger disconnect_arg ch)
      end
    
    class passive ~name ?max ?inactivity_timeout ?inactivity_check_interval ?pre_spawns ?step ~passive_arg ~disconnect_arg logger =
      object inherit [Conn.channel] Pool.t logger ~name ?max ?inactivity_timeout ?inactivity_check_interval ?pre_spawns ?step
                        ~create:(fun () -> Conn.passive_connect logger passive_arg)
                        ~finalise:(fun ch -> Conn.disconnect logger disconnect_arg ch)
      end
  end
  
