(*
 * Common library
 * pool.ml: generic pool
 *
 * (C) 2007 Alvise Spanò at H-Care srl.
 *)

(** Resource pooling facilities. *)
 
open Prelude 
open Printf 
 
(** Polymorphic resource pool. A pool is an object that arbitrates resources of type ['a] and access to them.
Resources can be acquired and released automatically through the [apply] method.
Depending on the availability of such resources, new ones will be created, or destroyed or acquiring will block until one becomes available.
[name] is the name of the pool (used for logging).
[max] optionally puts a cap to the number of resources that can be created and inserted into the pool.
[inactivity_timeout] optionally sets a timeout for inactive resources - after such timeout they are destroyed.
[inactivity_check_interval] sets the inactivity check timer for inactive resources.
[pre_spawns] sets the number of initial resources created at pool creation.
[step] sets the number of resources created at need - values higher than 1 are like a creation burst.
[create] is the creation function for the pooled resource.
[finalise] is the finalisation function for a resource that is scheduled for destruction upon inactivity.
[logger] is the logger for this pool.
*)
class ['a] t ~name ?max ?inactivity_timeout ?(inactivity_check_interval = 60.0) ?(pre_spawns = 0) ?(step = 1) ~create ~finalise logger =
  object (self)
    (**/**)
    val m = Mutex.create ()
  	val c = Condition.create ()
    val cnt = new XThread.mut 0
    val mutable l = []
    
  	method private pop =
  	    match l with
  	       []      -> None
  	     | x :: xs -> l <- xs; Some x
  	    
    method private push x = l <- x :: l
    
    method private filter p = l <- List.filter p l
    
    initializer
        for i = 1 to crop (0, match max with None -> pre_spawns | Some n -> n) pre_spawns do
            self#generate 1
        done;
        on_some (fun ito ->
            ignore (Timer.add_handler logger ~name:(sprintf "%s:poolwiper" name) ~interval:inactivity_check_interval `Reload (fun _ ->
                let now = Time.now () in
                Mutex.lock m;
                self#filter (fun (x, ts) -> if now -. ts > ito then (finalise x; cnt#apply_and_set (fun x -> x - 1); false) else true);
                Mutex.unlock m)))
            inactivity_timeout

    method private generate step =
        for i = 1 to step do
            self#push (create (), Time.now ());
            cnt#apply_and_set (fun x -> x + 1)
        done
    
    method private release x =
        Mutex.lock m;
        self#push (x, Time.now ());
        Mutex.unlock m;
        Condition.signal c
         
    method private acquire = 
         manage_resource
            (fun () -> Mutex.lock m)
            (fun () ->
                let rec recur () =
                    match self#pop with
                        None ->
                            (match max with
                                Some max when cnt#get = max -> Condition.wait c m
                                | _                         -> self#generate step);
                            recur ()
                  | Some (x, _) -> x
                in
                    recur ())
            (fun () -> Mutex.unlock m)
    (**/**)
            
    (** Acquires a resource and evaluates [f] passing it as parameter. Automatically releases the resource back to the pool afterwards. *)
    method apply : 'b. ('a -> 'b) -> 'b = fun f -> manage_resource (fun () -> self#acquire) f self#release
        
  end
  


