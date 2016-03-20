(*
 * Common library
 * xThread.ml: extended threads
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Stdlib Thread module extension. *)
 
open Prelude 
open Printf
open XList
 
(** Synchronized object encapsulator.
It stores [x] and arbitrates its access atomically following a functional paradigm instead of the typical error-prone lock-unlock mechanism.
*)
class ['a] sync x =
	let mlock m = Mutex.lock m
	and munlock m = Mutex.unlock m
	in
	  object (self)
		val m = Mutex.create ()
		val ownm = Mutex.create ()
		val mutable x = x
		val mutable owner = None
		
		method private lock =
			mlock ownm;
			match owner with
				Some (n, id) when id = Thread.id (Thread.self ()) ->
					owner <- Some (n + 1, id);
					munlock ownm
					
			  |	_ -> munlock ownm;
			  		 let id = Thread.id (Thread.self ()) in
			  		 mlock m;
			  		 mlock ownm;
			  		 owner <- Some (0, id);
			  		 munlock ownm
			  		
		method private unlock =
			mlock ownm;
			match owner with
			  | Some (0, id) when id = Thread.id (Thread.self ()) ->
			  		owner <- None;
			  		munlock ownm;
			  		munlock m
			  		
			  | Some (n, id) when id = Thread.id (Thread.self ()) ->
			  		owner <- Some (n - 1, id);
			  		munlock ownm
			  		
			  | _ -> munlock ownm; raise (Unexpected "sync#unlock")
		
		(** Atomically sets the stored object to [x']. *)
		method set x' =
			self#lock;
			x <- x';
			self#unlock
		
		(** Atomically evaluates [f] passing the stored object as parameter and replacing it with the return value of [f].*)
	  	method apply_and_set f =
			self#lock;
			(try x <- f x with e -> self#unlock; raise e);
			self#unlock
		
		(** Atomically evaluates [f] passing the stored object as parameter, replacing it with the first element of the pair returned by
		[f] and finally returning the second element. *)
		method set_and_apply : 'b. ('a -> ('a * 'b)) -> 'b = fun f ->
			self#lock;
			let (x', ret) = try f x with e -> self#unlock; raise e in
			x <- x';
			self#unlock;
			ret
		
		(** Atomically evaluates [f] passing the stored object as parameter. *)
	  	method apply : 'b. ('a -> 'b) -> 'b = fun f ->
			self#lock;
			let ret = try f x with e -> self#unlock; raise e in
			self#unlock;
			ret
	  end
	  
	  
(** Refinement of [sync] with the [get] method. Useful for creating quick synchronized mutable fields in objects or records.
*)
class ['a] mut x =
  object inherit ['a] sync x as super
    method get = super#apply ident
  end
	  
	  	
(** Typical object-oriented condition class.
*)  
class rendezvous =
  object
    val m = Mutex.create ()
  	val c = Condition.create ()
  	
  	(** Waits for the condition to be signaled. *)
  	method wait =
  	    Mutex.lock m;
  	    Condition.wait c m;
  	    Mutex.unlock m
 	   
 	(** Signals the condition and awakes the first thread that was waiting. *)
  	method signal = Condition.signal c
  	
  	(** Signals the condition awaking all the threads that were waiting. *)
  	method broadcast = Condition.broadcast c
  end 	
	  

(** Condition with data queue.
*)  
class ['a] condition =
  object
    val q = Queue.create ()
  	val m = Mutex.create ()
  	val c = Condition.create ()
  	
  	(** Waits for the condition to be signaled and returns the first value available. If some data is already available it returns
  	immediately popping from the queue.
  	*)
  	method wait : 'a =
  	    Mutex.lock m;
        while Queue.is_empty q do Condition.wait c m done;
        let r = Queue.pop q in
        Mutex.unlock m;
	    r 
	   
	(** Signals the condition and stores [x] in the queue. *)
  	method signal x =
  		Mutex.lock m;
  		Queue.push x q;
  		Mutex.unlock m;
  		Condition.signal c

  end  
  

(** Database of all threads. *)
let all_threads =
  object (self)
    val ths = new sync (Hashtbl.create 100)

    initializer self#register (Thread.self ()) "main"
    
    method register th name = ths#apply (fun tbl -> Hashtbl.add tbl (Thread.id th) (name, th, Time.now ()))
    method unregister id = ths#apply (fun tbl -> Hashtbl.remove tbl id)
    method rename id name = ths#apply (fun tbl -> let (_, th, t) = Hashtbl.find tbl id in Hashtbl.replace tbl id (name, th, t))
    method fold : 'z. ('z -> 'k -> 'x -> 'z) -> 'z -> 'z = fun f z -> ths#apply (fun tbl -> Hashtbl.fold (fun id x z -> f z id x) tbl z)
    method get_name id = ths#apply (fun tbl -> match (Hashtbl.find tbl id) with (name, _, _) -> name)
    method get_thread id = ths#apply (fun tbl -> match (Hashtbl.find tbl id) with (_, th, _) -> th)
    method get_create_time id = ths#apply (fun tbl -> match (Hashtbl.find tbl id) with (_, _, t) -> t)

    method length = ths#apply Hashtbl.length
  end

(** Get a thread name by id. *)
let name id =
	try all_threads#get_name id
	with Not_found -> "unknown"
         
(** Pretty prints a thread as [NAME\[ID\]]. *)
let pretty id = sprintf "%s[%d]" (name id) id

(** Creates a thread whose name is [name], registering to the global database and making it evaluate [f x]. *)
let create (logger : #Log.logger) name (f : 'a -> unit) (x : 'a) =
	let main x =
		let self = Thread.self ()
		in
		    all_threads#register self name;
			(try f x
			with Exit -> logger#debug "thread exited voluntarily" 
			  |  e    -> logger#fatal_error ~code:[1;4;2] (sprintf "thread fatal exception: %s" (pretty_exn e)));
			all_threads#unregister (Thread.id self)
	in
	let finally _ = logger#fatal_error ~code:[1;4;3] (sprintf "could not create thread %s for too long" name); exit 1
	in
	    retry_times ~times:23 ~finally 0 (fun n ->
    		try `Done (Thread.create main x)
    		with _ -> logger#warn ~code:[1;4;1] (sprintf "cannot create thread %s (%d currently active). Retrying in a short while..." name all_threads#length);
    		          let timeout = float_of_int (Math.fib n) /. 1000. in `Retry (timeout, n + 1))
    

(** Object-oriented view of threads.
A box is an object that traps a thread within it. Such thread can be used to for evaluating closures and it doesn't terminate of evaluation
termination and will be available for further evaluations.
*)
class box ~name logger =
  let make_name s = sprintf "%s[%s]" s name in
  object
    val cond = new condition
  
    initializer
        ignore (create logger (make_name "idle") (fun () ->
            let id = Thread.id (Thread.self ())
            in
                while true do
                    match cond#wait with
                        `Term   -> raise Exit
                      | `Exec (thname, f) ->
                            all_threads#rename id (make_name thname);
                            (try f ()
                            with Exit -> logger#debug "boxed thread exited voluntarily"
                              |  e    -> logger#fatal_error ~code:[1;4;4] (sprintf "boxed thread fatal exception: %s" (pretty_exn e)));
                            all_threads#rename id (make_name "idle");
                done) ())

    (** Evaluates [f x] and change self name to [name]. *)
    method exec : 'a. string -> ('a -> unit) -> 'a -> unit = fun name f x -> cond#signal (`Exec (name, fun () -> f x))
    
    (** Terminates the thread trapped within the box. *)
    method terminate = cond#signal `Term
  end

	(** Smarter version of Thread.delay. Does nothing if [x] is 0.0 *)
let delay = function 0.0 -> () | x -> Thread.delay x
