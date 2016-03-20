(*
 * HDA Bridge
 * test concurrent reading/writing
 *
 * (C) 2007 H-Care srl. 
 *)


open Prelude
open Printf


let iterations = 100
let pre_threads = 100
let threads = 1000
let writer_delay () = rnd_float (0.0001, 0.0005)
let reader_delay () = rnd_float (0.001, 0.002)

let logger = new LogLib.color_console_logger ()

let idle () =
    logger#msg Log.Low "idle";
    Unix.pause ()


let results : int list XThread.sync = new XThread.sync []

let check n = 
	let rec f = function
		[]				 	-> true
	  | x::[]				-> x = n
	  | x::xs when x = n 	-> f xs
	  | x::xs when x <> n	-> false
	  | _					-> raise (Prelude.Unexpected "boh")
	in
		match results#apply f with
			true	-> logger#msg Log.Normal "kk m8"
		  |	false	-> logger#warn Log.Normal "no ty"  

let data =
  object (self)
	val content = Buffer.create 20
	val c : unit XThread.condition = new XThread.condition
	val size = new XThread.sync 0
	val completed = new XThread.sync false

	method set_completed = completed#apply_and_set (fun _ -> true); c#broadcast ()

	method write s = 
		size#apply_and_set (fun n ->
			Buffer.add_string content s;
			c#broadcast ();
			n + (String.length s))

	method read (f : string -> unit) = 
		let seek = ref 0 in
		while not (completed#apply ident) do	
			size#apply (fun n ->
				f (Buffer.sub content !seek (n - !seek)); seek := n);
			c#wait 
		done;
		Buffer.contents content
  end

let __fresh = ref 0
let fresh () = __fresh := !__fresh + 1; !__fresh	

let create_writer () = 
	let f _ = 
		let rec g = function 
			0	-> ()
		  | n	-> data#write (string_of_int (fresh ())); Thread.delay (writer_delay ()); g (n - 1)
		in
			g iterations; data#set_completed
	in
		ignore (XThread.create logger "writer" f ())

let create_reader n = 
	let f _ = let s = data#read ignore in
			  let n = Hashtbl.hash s
			  in
				results#apply_and_set (fun l -> n :: l);
				check n
	in
		ignore (XThread.create logger (sprintf "reader%d" n) f ())


let rec create_readers = function
	0	-> ()
  | n	-> create_reader n; Thread.delay (reader_delay ()); create_readers (n - 1)

;;


create_readers pre_threads;

create_writer ();

create_readers threads;

idle ()



