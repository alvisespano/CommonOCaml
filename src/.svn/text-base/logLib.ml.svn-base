(*
 * Common library
 * loggerLib.ml: logger library
 *
 * (C) 2005 Quendi srl.
 *)

(** Logger library for Log. *)
 
open Prelude
open Printf
open Time
open Log
open XString


class ['a] qqueue quota =
  object
	val queue : 'a Queue.t XThread.sync = new XThread.sync (Queue.create ())

	method push x = 
		let f q = 
			Queue.push x q;
			if Queue.length q > quota then ignore (Queue.pop q)
		in
			queue#apply f

	method to_list = 
		let f = Queue.fold (fun acc x -> x :: acc) []
		in
			List.rev (queue#apply f)
  end

(** Typical logger base class.
Inherit from this virtual class and implement the virtual methods for string output.
[threshold] is the initial level threshold for log lines (default to [Debug]).
*)
class virtual logger_base ?(threshold = Debug) ?history_quota () =
  object (self)
  	val threshold = new XThread.mut threshold
	val history = match history_quota with 
	  	None   -> None
	  | Some 0 -> None
	  | Some n -> Some (new qqueue n)

	method private history_log kind ~code s = match history with 
		None   -> ()
	  | Some q -> q#push (Time.now (), kind, code, XThread.pretty (Thread.id (Thread.self ())), s)
    
    method private virtual log_debug 			: string -> unit
    method private virtual log_info				: string -> unit
    method private virtual log_warn 			: code:(int list) -> string -> unit
    method private virtual log_error 			: code:(int list) -> string -> unit
    method private virtual log_fatal_error 		: code:(int list) -> string -> unit
	method private virtual log_unmaskerable		: string -> unit
    
  	method debug s = if threshold#apply (compare_severity Debug) >= 0 
					 then (self#history_log Log.Debug ~code:None s; self#log_debug s)

  	method info s = if threshold#apply (compare_severity Info) >= 0 
					then (self#history_log Log.Info ~code:None s; self#log_info s)

   	method warn ~code s = if threshold#apply (compare_severity Warn) >= 0 
						  then (self#history_log Log.Warn ~code:(Some code) s; self#log_warn ~code s)

  	method error ~code s = if threshold#apply (compare_severity Error) >= 0 
					 	  then (self#history_log Log.Error ~code:(Some code) s; self#log_error ~code s)

	method fatal_error ~code s = if threshold#apply (compare_severity Fatal_error) >= 0
						  		then (self#history_log Log.Fatal_error ~code:(Some code) s; self#log_fatal_error ~code s)

	method unmaskerable s = if threshold#apply (compare_severity Unmaskerable) >= 0
						    then (self#history_log Log.Unmaskerable ~code:None s; self#log_unmaskerable s)
    
    method set_threshold msg_kind = threshold#set msg_kind
    
    method get_threshold = threshold#get 

	method history sev limit : Log.history_log_line list option = match history with 
		None	-> None
	  | Some q	-> 
		  begin
		  	let l = q#to_list in 
		  	let f (_, sev', _, _, _) = 
				(match sev with 					
				    None 		  -> true
				  | Some sev -> Log.compare_severity sev' sev >= 0)
		   	in
				let l' = List.filter f l in 
				let len = List.length l' 
				in
					match limit with 
						None   				 -> Some l'
					  | Some n when n >= len -> Some l'
					  | Some n 				 -> Some (XList.tail n l') (* todo: controllare *)
		  end
						
						
  end
    

(** Creates a logger from two loggers. The resulting logger logs on both loggers given as parameters upon construction.
*)
class double_logger ?threshold ?history_quota (l1 : #logger) (l2 : #logger) =
  object inherit logger_base ?threshold ?history_quota () 
  	method private log_debug s = l1#debug s; l2#debug s
    method private log_info s = l1#info  s; l2#info s
   	method private log_warn ~code s= l1#warn ~code s; l2#warn ~code s
  	method private log_error ~code s = l1#error ~code s; l2#error ~code s
	method private log_fatal_error ~code s = l1#fatal_error ~code s; l2#fatal_error ~code s
	method private log_unmaskerable s = l1#unmaskerable s; l2#unmaskerable s
  end


(** The null logger.
*)
class null_logger ?history_quota () =
  object inherit logger_base ?history_quota ()
    method private log_debug _ = ()
    method private log_info _ = ()
    method private log_warn ~code:_ _ = ()
    method private log_error ~code:_ _ = ()
    method private log_fatal_error ~code:_ _ = ()
	method private log_unmaskerable _ = ()
  end


(** Logger wrapper that can be used for changing logger at runtime.
The initial logger is given as parameter.
*)
class mutable_logger (l : #logger) =
  object
    val logger = new XThread.mut (l :> logger)
    
    (** Sets the new logger. *)
    method set (l : #logger) = logger#set l
    
    method info = logger#get#info
   	method warn = logger#get#warn
  	method debug = logger#get#debug
	method error = logger#get#error
	method fatal_error = logger#get#fatal_error
    
    method set_threshold = logger#get#set_threshold
    method get_threshold = logger#get#get_threshold

	method history = logger#get#history
  end


(** Creates a logger, given a logger as parameters, where string [pre] is prefixed at each log line.
Format of the prefix can be changed with the optional parameter [format] - such format must support two string inputs, the former being
the prefix string and the latter the message body.
*)
let prefixer (l : #logger) ?(format = format_of_string "%s: %s") pre =
  object
   	method debug s = l#debug (sprintf format pre s)
    method info s = l#info (sprintf format pre s)
   	method warn ~code s = l#warn ~code (sprintf format pre s)
   	method error ~code s = l#error ~code (sprintf format pre s)
   	method fatal_error ~code s = l#fatal_error ~code (sprintf format pre s)
	method unmaskerable s = l#unmaskerable (sprintf format pre s)
    
    method set_threshold = l#set_threshold
    method get_threshold = l#get_threshold

	method history = l#history
  end

(** Logger on stdlib out channel.
[out] is the outchannel.
[pretty_now] is the pretty printing function for the current time.
*)
class channel_logger ~out ?(pretty_now = fun () -> pretty_datetime (Time.now ()))
                     ?threshold ?history_quota () =
  object (self) inherit logger_base ?threshold ?history_quota ()
    
    val out = new XThread.sync out	    
		    
	method private print ~level ?code txt =
		let now = pretty_now () in
		let th = XThread.pretty (Thread.id (Thread.self ())) in
    	let code = match code with 
			None
 		  | Some [] -> "" 
		  | Some l	-> (sprintf " %s" (XString.mappen_strings ~sep:":" string_of_int l))
        in
			out#apply (fun out -> fprintf out "%s %s: [%s%s] %s\n%!" now th level code txt)

    method private log_debug s = self#print ~level:"DEBUG" s
    method private log_info s = self#print ~level:"INFO" s
    method private log_warn ~code s = self#print ~level:"WARN" ~code s
    method private log_error ~code s = self#print ~level:"ERROR" ~code s
    method private log_fatal_error ~code s = self#print ~level:"FATALERROR" ~code s
	method private log_unmaskerable s = self#print ~level:"UNMASKERABLE" s
    end
  
  
(** Logger on file channels.
*)
class file_logger ~filename ?pretty_now ?threshold ?history_quota () =
	let out = open_out_gen [Open_wronly; Open_append; Open_creat] 0o664 filename 	
	in
	  object inherit channel_logger ?pretty_now ?threshold ~out ?history_quota ()		
	  end  


(** Logger on file channels with roller.
*)
class file_roller_logger 
		~filename 
		?(max_size = 0) 
		?(max_files = 10) 
		?(pretty_now = fun () -> pretty_datetime (Time.now ()))
		?threshold 
		?history_quota () =
	let create_chan () = 
		let out = open_out_gen [Open_wronly; Open_append; Open_creat] 0o664 filename in
		let size = (Unix.stat filename).Unix.st_size 
		in
			(out, size)
	in
	  object (self) inherit file_logger ~filename ~pretty_now ?threshold ?history_quota () as super
		val chan = new XThread.sync (create_chan ())
		val mutable latest_file = ref 0
		    
		method private print ~level ?code txt = match max_size with
			0 	-> super#print ~level ?code txt
		  | n	-> 
			  begin
		    	let code = match code with 
					None
		 		  | Some [] -> "" 
				  | Some l	-> (sprintf " %s" (XString.mappen_strings ~sep:":" string_of_int l))
		        in
				let s = sprintf "%s %s: [%s%s] %s" (pretty_now ()) (XThread.pretty (Thread.id (Thread.self ()))) level code txt in
				let f (out, size) = 	
					fprintf out "%s\n%!" s; 
					if size > n then
					  begin 
					  try 
						close_out out;
						Unix.rename filename (sprintf "%s.%d" filename !latest_file);
						latest_file := if !latest_file = max_files - 1 then 0 else !latest_file + 1;
						create_chan ()
					  with e -> 
						begin
							self#error 
								~code:[1;8;1]
								(sprintf "exception caugh while rolling logfile: %s. Continue using old file." (pretty_exn e));
							(out, size + (String.length s))
						end
					  end
					else (out, size + (String.length s))
				in
					chan#apply_and_set f
					
			  end
		  end  

  

(** Logger on stdout channel.
*)
class console_logger ?pretty_now ?threshold ?history_quota () =
  object inherit channel_logger ?pretty_now ?threshold ?history_quota ~out:stdout ()
  end


(**/**)	
	
type color = Black | Red | Blue | Green | Magenta | Yellow | Cyan | White

type brightness = Dark | Bright

let esc_code_of_brightness = function
	Dark   -> 2
  | Bright -> 1

let esc_code_of_color = function
	Black 	-> 30
  | Red	  	-> 31
  | Green 	-> 32
  | Yellow	-> 33
  | Blue  	-> 34
  | Magenta -> 35
  | Cyan	-> 36
  | White 	-> 37

let esc = function
	[] -> ""
  | l  -> sprintf "\027[%sm" (mappen_strings ~sep:";" string_of_int l)
  
let enclose l1 l2 s = sprintf "%s%s%s" (esc l1) s (esc l2)

let brighten s l = enclose [1] l s

let darken s l = enclose [2] l s

(**/**)

(** Logger on stdout channel with colors for color consoles. *)
class color_console_logger ?(pretty_now = fun () -> pretty_datetime (Time.now ())) 
						   ?threshold ?history_quota () =
  object (self) inherit logger_base ?threshold ?history_quota ()
  	
  	val stdout = new XThread.sync stdout
            		
	method private print ~level ?code ?color ?brightness txt =
		let now = pretty_now () in
		let th = brighten (XThread.pretty (Thread.id (Thread.self ()))) [0] in
    	let code = match code with 
			None
 		  | Some [] -> "" 
		  | Some l	-> (sprintf " %s" (XString.mappen_strings ~sep:":" string_of_int l))
        in
		let level_and_code = brighten 
								(sprintf "[%s%s]" level code) 
								(0 :: (match color with Some c -> [esc_code_of_color c] | None -> [])) 
		in
		let escs = match color with
		    None   -> []
		  | Some c -> [esc_code_of_color c]
		in
		let escs = match brightness with
		    None   -> escs
	      | Some b -> (esc_code_of_brightness b) :: escs		
        in
		let txt = enclose escs [0] (sprintf "%s %s" level_and_code txt)
		in
        	stdout#apply (fun stdout -> fprintf stdout "%s %s: %s\n%!" now th txt)
 
    method private log_debug s = self#print ~brightness:Dark ~level:"DEBUG" ~color:Green s
    method private log_info s = self#print ~level:"INFO" s
    method private log_warn ~code s = self#print ~brightness:Bright ~level:"WARN" ~code ~color:Yellow s
    method private log_error ~code s = self#print ~brightness:Bright ~level:"ERROR" ~code ~color:Red s
    method private log_fatal_error ~code s = self#print ~brightness:Bright ~level:"FATALERROR" ~code ~color:Magenta s
	method private log_unmaskerable s = self#print ~brightness:Bright ~level:"UNMASKERABLE" ~color:Cyan s
  end



