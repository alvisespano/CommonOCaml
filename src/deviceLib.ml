(*
 * Common library
 * deviceLib.ml: device library
 *
 * (C) 2005 Quendi srl.
 *)
 
(** High-level library for Device. *) 
 
open Prelude
open Security
open Cleanup
open Printfopen Device
open Utility
      		       		 
      		 (** Creates a synchronized [rw] device given any [rw] device. Synchronization is global for each operation, i.e. any operation will lock the device.
*)  
class ['key, 'data] sync_rw (rw : ('key, 'data) #rw) =
  object
  	val sy = new XThread.sync ()
  	method load k = sy#apply (fun () -> rw#load k)
  	method save k data = sy#apply (fun () -> rw#save k data)
  	method exists k = sy#apply (fun () -> rw#exists k)
  	method size k = sy#apply (fun () -> rw#size k)
  	method flush = sy#apply (fun () -> rw#flush)
  	method delete k = sy#apply (fun () -> rw#delete k)
  	method clean = sy#apply (fun () -> rw#clean)
  end
    		
(** Creates a synchronized [rwr] device given any [rwr] device. Synchronization is global for each operation, i.e. any operation will lock the device.
*)  
class ['key, 'data] sync_rwr (rwr : ('key, 'data) #rwr) =
  object inherit ['key, 'data] sync_rw rwr
  	method poll = sy#apply (fun () -> rwr#poll)
  end


(** Creates an [rw] device that flushes itself every [timeout] seconds given any [rw] device.
*)
class ['key, 'data] autoflushed logger timeout (rw : ('key, 'data) #rw) =
	let update () = rw#flush
	in
	  object inherit destructible
	  	val tid = Timer.add_handler logger ~name:"autoflushed" ~interval:timeout `Reload (fun _ -> update ())
	  		
	  	method destructor =	Timer.remove_handler tid
		method load = rw#load
		method save = rw#save
		method exists = rw#exists
		method size = rw#size
		method flush = rw#flush
		method delete = rw#delete
		method clean = rw#clean
	  end

		
(** Creates a [rw] device with a cache. The cache size (in bytes) is [cache_size], the size function for stored data is passed as [size_of].
The caching policy is LRU and a cache flush is guaranteed on object collection or program exit.
*)      		 
class ['key, 'data] cached_rw size_of cache_size (rw : ('key, 'data) #rw) =
  object (self) inherit destructible
  	val cache =
		let access =
		  object
		  	method load = rw#load
		  	method store = rw#save
		  	method size_of = size_of
		  end
		in
			new Cache.lru ~size:cache_size access
	
	method destructor = self#flush 
			
	method load = cache#read
	method save = cache#writethrough
	method exists = rw#exists
	method size = rw#size
	method flush = cache#push_all; rw#flush
	method delete k = cache#inv k; rw#delete k
	method clean = cache#flush_all; rw#clean
  end

(** Creates a [rwr] device with a cache.
*)
class ['key, 'data] cached_rwr logger size_of cache_size (rwr : ('key, 'data) #rwr) =
  object (self) inherit ['key, 'data] cached_rw size_of cache_size rwr
  	method destructor =
  		try self#flush
  		with e -> logger#warn (sprintf "couldn't successfully flush cached removable device on destruction due to exception: %s" (pretty_exn e))
  		
  	method poll = rwr#poll
  end
  
	
(** Creates a devices that acts as a mirror given two devices. The first device is the reference device, the second is the mirror device.
*)
class ['key, 'data] mirrored logger (src : ('key, 'data) #rw) (dst : ('key, 'data) #rw) =
	let src = (src :> ('key, 'data) rw) in
	let dst = (dst :> ('key, 'data) rw)
	in
	  object (self)
	    (** Loads from reference device; if it fails then mirror is used. *)
	  	method load k =
	  		match (src#exists k, dst#exists k) with
	  			(true, true) ->
	  				(try
	  					let data = src#load k
	  					in
	  						(try if data = dst#load k then () else dst#save k data
	  						with Failure (dev, s) -> logger#warn  (sprintf "mirrored: couldn't update mirror on %s device: %s" dev s));
	  						data
	  				with Failure (dev, s) ->
	  						logger#warn (sprintf "mirrored: couldn't load from reference device %s (%s). Loading from mirror" dev s);
	  						dst#load k)
	  		  
	  		  | (true, false) ->
	  		  		let data = src#load k
	  		  		in
	  		  			(try dst#save k data
	  		  			with Failure (dev, s) -> logger#warn (sprintf "mirrored: couldn't save on mirror device %s: %s" dev s));
	    		  		data
	  		  
	  		  | (false, true) ->
	  		  		let data = dst#load k
	  		  		in
	  		  			(try src#save k data
	  		  			with Failure (dev, s) -> logger#warn (sprintf "mirrored: couldn't save on reference device %s: %s" dev s));
	    		  		data
	  		  
	  		  | (false, false) -> raise (Failure ("mirrored", "not found"))
	  		
	  	method private both f msg =
	  		let a = try f src; None
	  				with Failure (dev, s) -> Some (sprintf "%s reference device %s: %s" msg dev s)
			in
			let b = try f dst; None
					with Failure (dev, s) -> Some (sprintf "%s mirror device %s: %s" msg dev s)
			in
				match (a, b) with
					(Some s1, Some s2) -> raise (Failure ("mirrored", sprintf "%s both reference device (%s) and mirror device (%s)" msg s1 s2))
				  | (Some s, None)
				  | (None, Some s)	   -> logger#info (sprintf "mirrored: %s" s)
				  | (None, None)	   -> ()
	
	    (** Saves on both devices (if possible). *)
	 	method save k data = self#both (fun rw -> rw#save k data) "couldn't save on"

        (** Queries for existance in any of the two devices. *)	 			
	  	method exists k = src#exists k || dst#exists k

		method size k = match (src#exists k, dst#exists k) with
			(true, _)		-> src#size k
		  | (_, true)		-> dst#size k
		  | (false, false)	-> raise (Failure ("mirrored", "not found"))
	  	
	  	(** Flush both devices. *)
	  	method flush = self#both (fun rw -> rw#flush) "couldn't flush"
	  	
	  	(** Delete on both devices. *)
	  	method delete k = self#both (fun rw -> rw#delete k) "couldn't delete on"
	  	
	  	(** Clean both devices. *)
	  	method clean = self#both (fun rw -> rw#clean) "couldn't clean"
	  end

  
(** Creates a [rw] device given a [rwr] device handling polling automatically.
*)   			
class ['key, 'data] smart_removable (logger : #Log.logger) ~polling_timeout ~update_pri (rwr : ('key, 'data) #rwr) =
	let chain = new event_chain logger in
	let rec h1 tid =
		if not (rwr#poll) then begin
			chain#notify `Removed;
			Timer.change_handler tid `Reload h2 ~interval:polling_timeout
		  end
	and h2 tid =
		if rwr#poll then begin
			chain#notify `Inserted;
			Timer.change_handler tid `Reload h1 ~interval:polling_timeout
		  end
	in
	  object (self) inherit destructible
		val id = Timer.add_handler logger ~interval:polling_timeout ~name:"smart_removable" `Reload h1
		val actions = Queue.create ()
		
		initializer
			chain#add_listener `Inserted ~pri:update_pri (fun () -> self#update)
			
		method private update =
			while not (Queue.is_empty actions) do
				try (Queue.pop actions) ()
				with Exit -> ()
				  |  e    -> logger#warn ~code:[1;7;1] (sprintf "smart_removable: exception %s trapped while performing actions" (pretty_exn e))
			done	
						
		method private enqueue f =
			Queue.add f actions;
			if rwr#poll then self#update
		
		method destructor =
			Timer.remove_handler id;
			try self#update
			with _ -> logger#warn ~code:[1;7;2] "couldn't successfully update smart removable device on destruction"
				
	    (** Invoke [hook] on device removal. *)			
		method on_removal ?pri hook =
			chain#add_listener `Removed hook ?pri;
			if not rwr#poll then hook ()
		
		(** Invoke [hook] on device insertion. *)		
		method on_insertion ?pri hook =
			chain#add_listener `Inserted hook ?pri;
			if rwr#poll then hook ()
			
		(* rw *)
		
	  	method load = rwr#load
	  	
	 	method save url data = self#enqueue (fun () -> rwr#save url data)
	 		
	 	method exists url = rwr#poll && rwr#exists url

	 	method size = rwr#size
	 	
	 	method flush = self#enqueue (fun () -> rwr#flush)
	 	
	 	method delete k = self#enqueue (fun () -> rwr#delete k)
			
		method clean = self#enqueue (fun () -> rwr#clean)
	  end
  
  
(** Type for encrypted and compressed data used by secure devices. *)
type 'a secure = 'a zip cipher

(**/**)
let ensecure secret x : 'a secure = encrypt secret (crunch x)

(**/**)
let desecure secret (sec : 'a secure) = decrunch (decrypt secret sec)

(** Creates a secure [rw] device given any [rw] device. Secret [secret] is used for both data and key encryption. The function [encode_key] is
used for function key encryption.
*)
class ['key, 'data] secure_rw secret encode_key (rw : ('key, 'data secure) #rw) =
  object
	method load k = desecure secret (rw#load (encode_key secret k))
	method save k data = rw#save (encode_key secret k) (ensecure secret data)
	method exists k = rw#exists (encode_key secret k)
	method size = rw#size 
	method flush = rw#flush
	method delete (k : 'key) = rw#delete (encode_key secret k)
	method clean = rw#clean
  end  

(** Creates a secure [rwr] device given any [rwr] device. Data is compressed and encrypted with a 3DES algorithm.
Secret [secret] is used for both data and key encryption. The function [encode_key] is
used for function key encryption.
*)
class ['key, 'data] secure_rwr secret encode_key (rwr : ('key, 'data secure) #rwr) =
  object inherit ['key, 'data] secure_rw secret encode_key rwr
    method poll = rwr#poll
  end
   
 
(** Data type used by consistent devices. *)
type 'a consistent = Consistent of Digest.t * string

(**/**)
let enconsist (x : 'a) : 'a consistent = let x = marsh x in Consistent (Digest.string x, x)
(**/**)

(**/**)
let deconsist (Consistent (md5, x) : 'a consistent) : 'a option = 
	if Digest.string x = md5 then Some (unmarsh x) else None
(**/**)

(** Creates a consistent [rw] device given any [rw] device. Data is stored together with an MD5 digest for verifying its consistency at load time.
*)
class ['key, 'data] consistent_rw (rw : ('key, 'data consistent) #rw) =
  object
	method load k =
		try
			match deconsist (rw#load k) with
				Some data -> data
			  | None      -> raise (Failure ("consistent_rw", "inconsistent data"))
		with _ -> raise (Failure ("consistent_rw", "currupted data"))
			  	
	method save k data = rw#save k (enconsist data)
	method exists = rw#exists
	method size = rw#size
	method flush = rw#flush
	method delete = rw#delete
	method clean = rw#clean
  end  

(** Creates a consistent [rwr] device given any [rwr] device. Data is stored together with an MD5 digest for verifying its consistency at load time.
*)
class ['key, 'data] consistent_rwr (rwr : ('key, 'data consistent) #rwr) =
  object inherit ['key, 'data] consistent_rw rwr
    method poll = rwr#poll
  end
  	
  
(* backup device *)  
	
(** Data type used by backup devices. *)
type 'a backup = Backup of float * 'a

(**/**)
let enbackup x = Backup (Unix.gettimeofday (), x)
(**/**)

(** Creates a backup [rw] device given any [rw] device. Data is stored redundantly and backups are retrived in case of failure at load time.
Function [backup_key] for creating a backup key given a key must be passed.
*)
class ['key, 'data] backup_rw logger backup_key (rw : ('key, 'data backup) #rw) =
  object (self)
  	val tbl = Hashtbl.create 23
  
  	method private retrieve k =
  		try Former (rw#load k)
  		with e -> Latter (pretty_exn e)
  	
	method private first_load k =
		let (k1, k2) = (k, backup_key k) in
		let (k1, k2, data) =
			match (rw#exists k1, rw#exists k2) with
				(false, false) -> raise (Failure ("backup_rw", "not found"))
			  | (true, false)  -> (k1, k2, let Backup (_, data) = rw#load k1 in data)
			  | (false, true)  -> (k2, k1, let Backup (_, data) = rw#load k2 in data)
			  |	(true, true) ->
					match (self#retrieve k1, self#retrieve k2) with
						(Former (Backup (tm1, data1)), Former (Backup (tm2, data2))) ->
							if tm1 > tm2 then (k1, k2, data1) else (k2, k1, data2)
					
					  | (Former (Backup (_, data)), Latter s) ->
					  		logger#info (sprintf "backup_rw: couldn't load backup copy (%s). Using original as reference" s);
					  		(k1, k2, data)
					  
					  | (Latter s, Former (Backup (_, data))) ->
					  		logger#info (sprintf "backup_rw: couldn't load original copy (%s). Using backup as reference" s);
					  		(k2, k1, data)
					  
					  | (Latter s1, Latter s2) ->
					  		raise (Failure ("backup_rw", sprintf "couldn't load either original (%s) or backup copy (%s)" s1 s2))
		in
			Hashtbl.add tbl k (k1, k2);
			data
		
	method load k =
  		try
  			let (k1, k2) = Hashtbl.find tbl k
	  		in
	  			try let Backup (_, data) = rw#load k1 in data
	  			with Failure (dev, s) ->
	  				logger#info (sprintf "backup_rw: couldn't retrieve latest copy on device %s (%s). Loading backup copy" dev s);
	  			  	let Backup (_, data) = rw#load k2 in
	  			  	Hashtbl.replace tbl k (k2, k1);
	  			  	data
		with Not_found -> self#first_load k
			  	
  	method save k data =
  		let (k1, k2) = try Hashtbl.find tbl k with Not_found -> (backup_key k, k)
  		in
  			rw#save k2 (enbackup data);
  			Hashtbl.replace tbl k (k2, k1)
  
  	method exists = rw#exists
	method size = rw#size
	
  	method flush = rw#flush
  	
  	method delete k =
  		Hashtbl.remove tbl k;
  		rw#delete k;
  		rw#delete (backup_key k)
  		
  	method clean = Hashtbl.clear tbl; rw#clean
  			
  end
  
(** Creates a backup [rwr] device given any [rwr] device. Data is stored redundantly and backups are retrived in case of failure at load time.
Function [backup_key] for creating a backup key given a key must be passed.
*)
class ['key, 'data] backup_rwr logger backup_key (rwr : ('key, 'data backup) #rwr) =
  object inherit ['key, 'data] backup_rw logger backup_key rwr
    method poll = rwr#poll
  end  

  
  
(** Creates data in the format used by resident objects. A custom [marshal] function can be passed in place of the default polymorphic one. *)	  
let create_resident_data ?(marshal = marsh) (version : int) o = (version, marshal o)
 
(** Resident objects are data objects that are stored on the given [rw] device every [timeout] seconds.
Custom [marshal] and [unmarshal] functions can be passed.
Optionally, non-naive [init] and [cleanup] hooks can be passed - the former is calles at data retrieval, the latter at object collection or at program exit.
If the optional constructor is passed, data is recreated in case of failure at retrieval time; an exception is raised otherwise.
*)
class ['a] resident (logger : #Log.logger) ~timeout
					?(marshal = marsh) ?(unmarshal = unmarsh)
					?(init = ignore) ?(cleanup = ignore)
					?cons name version (rw : (string, string) #rw) =
	let load name =
		try
			let (ver, o) = unmarsh (rw#load name)
			in
				if ver = version then Former (ver, unmarshal o)
				else Latter (sprintf "incompatible version %d" ver)
		with e -> Latter (sprintf "exception caught: %s" (pretty_exn e))
	in
	let save name data =
		try rw#save name (marsh (version, data))
		with e -> raise (Failure ("resident", sprintf "couldn't save resident object %s: %s" name (pretty_exn e)))
	in
	let (o, md5) =
		match load name with
			Former (ver, o) ->
				logger#info (sprintf "found resident object %s v%d" name ver);
				(o, Digest.string (marshal o))
				
		  | Latter txt ->
				let s = sprintf "cannot retrieve resident object %s: %s" name txt
				in
					match cons with
						None 	  -> raise (Failure ("resident", s))
				  	  | Some cons -> logger#info (sprintf "%s. Creating new" s);
			  						 (cons (), "")
	in
	let _ = init o in
	let obj = new XThread.sync (o, md5) in
	let update () =
		obj#apply_and_set (fun (o, h) ->
			let data = marshal o in
			let h' = Digest.string data
			in
				if h' <> h then save name data;
				(o, h'))
	in
	  object (self) inherit destructible
	  	val id = Timer.add_handler logger ~name:(sprintf "%s:residentupdater" name) ~interval:timeout `Reload (fun _ -> update ())
		
	  	method destructor =
	  		Timer.remove_handler id;
	  		self#apply cleanup;
	  		update ()
	  	
	  	(** Atomically evaluates the passed closure passing the resident data object as parameter. *)
	  	method apply : 'b. ('a -> 'b) -> 'b = fun f -> obj#apply (fun (o, _) -> f o)
	
	    (** Atomically evaluates the passed closure passing the resident data object as parameter and replaced the data with its return value.*)
	  	method apply_and_set f = obj#apply_and_set (fun (o, h) -> (f o, h))

        (** Atomically sets resident data to the passed value. *)	  		
	  	method set o = obj#apply_and_set (fun (_, h) -> (o, h))
	  	
	  	(** Forces an update of the resident data onto the target device. *)
	  	method update = update ()
    
        (** Get redident name. *)	  	
	  	method name = name
	  	
	  	(** Forces the destruction of the resident data and the self object as well. *)
	  	method wipe =
	  		Timer.remove_handler id;
	  		rw#delete name;
	  		obj#apply fst
	  end 
		
	  	 		  
(** Disk-like [rw] device that behaves like a Unix filesystem on a subdirectory.
[root] is the directory path.
[root_mode] is the creation permissions in case the directory does not exist.
[file_mode] is the creation permissions of ordinary files stored.
The key type is a [string] for the file path.
The data type is a [string] with raw data.
*)
class raw_disk ~root_mode ~file_mode root =
	let path url = Filename.concat root url
	in
	  object (self)
	  	initializer
			if Sys.file_exists root then ()
			else try Unix.mkdir root root_mode
				 with e -> raise (Failure ("raw_disk", sprintf "cannot create root dir %S: %s" root (pretty_exn e)))
	
	  	method load url =
	  		try Io.load_file (path url)
	  		with e -> raise (Failure ("raw_disk", sprintf "cannot load file %S: %s" (path url) (pretty_exn e)))
	  		
	  	method save url data =
	  		try Io.save_file (path url) file_mode data
	  		with e -> raise (Failure ("raw_disk", sprintf "cannot save file %S: %s" (path url) (pretty_exn e)))
	  		
	  	method exists url = Sys.file_exists (path url)

		method size url = (Unix.stat (path url)).Unix.st_size
	  	
	  	method flush = ()
	  	
	  	method delete url = try Io.delete_file (path url) with _ -> ()
	  	
	  	method foreach f = Array.iter f (Sys.readdir root)
	  	
	  	method clean = self#foreach self#delete
	  end 
 
	  
(** Disk-like [rw] device that behaves like a Unix filesystem on a subdirectory.
[root] is the directory path.
[root_mode] is the creation permissions in case the directory does not exist.
[file_mode] is the creation permissions of ordinary files stored.
The key type is a [string] for the file path.
The data type is any time and polymorphic marshallers are used for storing data.
*)  	
class ['data] disk ~root_mode ~file_mode root =
  object
  	val raw = new raw_disk ~root_mode ~file_mode root
    
  	method load url : 'data = unmarsh (raw#load url)
  	method save url (data : 'data) = raw#save url (marsh data)
  	method exists = raw#exists
	method size = raw#size
  	method flush = raw#flush
  	method delete = raw#delete
	method foreach = raw#foreach
  	method clean = raw#clean
  end
 	 

(** Raw disk [rw] device that cleans itself up on collection or program exit. *)
class temp_raw_disk ~root_mode ~file_mode root =
  object (self)
  inherit destructible
  inherit raw_disk ?root_mode ?file_mode root
  
  	initializer self#clean 
  
  	method destructor =
  		try	self#clean;	Unix.rmdir root
  		with _ -> ()
  end


(** Unbuffered version of the raw_disk device.
*)
class unbuffered_raw_disk ~root_mode ~file_mode root =
	let path url = Filename.concat root url
	in
	  object inherit raw_disk ~root_mode ~file_mode root 
		method load url = 
		  try 
			let fd = Unix.openfile (path url) [Unix.O_RDONLY] 0 in 
			let b = Buffer.create (kilo 40) in
			let rec f () =
				let s = String.create (kilo 4) in 
				let n = Unix.read fd s 0 (kilo 4) 
				in
					Buffer.add_string b s;			
					if n = 0 then Buffer.contents b else f ()
			in
				let contents = f ()
				in
					Unix.close fd;
					contents
	  	  with e -> raise (Device.Failure ("unbuffered_raw_disk", sprintf "cannot load file %S: %s" (path url) (pretty_exn e)))
	  end

(** [rw] device with dummy save methods.
*)
class ['key, 'data] read_only_rw (rw : ('key, 'data) #Device.rw) =
  object 	
	method load = rw#load
	method save (_ : 'key) (_ : 'data) : unit = raise (Failure ("read_only_rw", "could not save in a read only device"))
	method exists = rw#exists
	method size = rw#size
	method flush : unit = raise (Failure ("read_only_rw", "could not flush a read only device"))
	method delete (_ : 'key) : unit = raise (Failure ("read_only_rw", "could not delete in a read only device"))
	method clean : unit = raise (Failure ("read_only_rw", "could not clean a read only device"))
  end

(** Cached [rw] device with time expiration on cache entries. [decay] is the time decay for expiration.
*)
class ['key, 'data] cached_load_rw decay (rw : ('key, 'data) #rw) =
  object (self)
	val tbl : ('key, (float * 'data)) Hashtbl.t = Hashtbl.create 10

	method private store k = 
		let data = rw#load k 
		in 
			Hashtbl.replace tbl k (Time.now (), data);
			data
		
	method load (k : 'key) : 'data =
   	  try 
		let (t, data) = Hashtbl.find tbl k 
		in
			if (Time.now ()) -. t < decay 
			then data
			else self#store k
	  with Not_found -> self#store k

	method save k data = 
		Hashtbl.replace tbl k (Time.now (), data);
		rw#save k data

	method exists = rw#exists

	method size = rw#size

	method flush = 
		let f k (_, data) = rw#save k data in 
		Hashtbl.iter f tbl; 
		rw#flush
		
	method delete k = Hashtbl.remove tbl k; rw#delete k	

	method clean = Hashtbl.clear tbl; rw#clean

  end

class ['key, 'data] translator 
						~(string_of_key : 'key -> string) 
						~(string_of_data : 'data -> string) 
						~(data_of_string : string -> 'data) rw : ['key, 'data] Device.rw =
  object
	method load (k : 'key) : 'data 				= data_of_string (rw#load (string_of_key k))
	method save (k : 'key) (x : 'data) : unit 	= rw#save (string_of_key k) (string_of_data x) 
	method exists (k : 'key) : bool 			= rw#exists (string_of_key k)
	method size (k : 'key) : int				= rw#size (string_of_key k)
 	method flush : unit 						= rw#flush
 	method delete (k : 'key) : unit 			= rw#delete (string_of_key k)
 	method clean : unit 						= rw#clean
  end

class ['key, 'data] limbo 
						~(rw : ('key, 'data) #Device.rw) 
						~(trash : ('key, 'data) #Device.rw) =
  object 
	method load k = try rw#load k with e -> (try let x = trash#load k in rw#save k x; x with _ -> raise e)
	method save k x = rw#save k x
	method exists k = rw#exists k || trash#exists k
	method size k = if rw#exists k then rw#size k else trash#size k
 	method flush = rw#flush; trash#flush
 	method delete k = trash#save k (rw#load k); rw#delete k
 	method clean = rw#clean; trash#clean
  end

class deep_path_raw_disk ~root_mode ~file_mode root =
	let path url = Filename.concat root url
	in
	  object (self) inherit raw_disk ~root_mode ~file_mode root as super

		method private create_dir url =
			let split s = Str.split (Str.regexp "[/]+") s in
			let rec fold acc = function
				[]	   
			  | _ :: [] -> ()
			  | s :: ss ->
				  begin
					let dir = Filename.concat acc s in
					let path = Filename.concat root dir 
					in
						if not (Sys.file_exists path) then Unix.mkdir path root_mode; 
						fold dir ss
				  end
			in
				fold "" (split url)
		
		method save url data = 
			(match Sys.file_exists (Filename.basename (path url)) with
				true  -> ()
			  | false -> (try self#create_dir url 
						  with e -> raise (Device.Failure ("Storage.raw_disk", 
															sprintf "cannot create dir tree for file %s: %s" 
																(path url) (pretty_exn e)))));
			super#save url data

		method clean = ()
		method destructor = ()
	  end


