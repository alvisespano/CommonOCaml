(*
 * Common library
 * prelude.ml: misc stuff
 *
 * (C) 2005 Quendi srl.
 *)

(** Misc stand-alone utilities. This module, as the name suggests, somehow behaves as a starter for the whole library. *)

open Printf

(** Unexpected exception: to be raised when something beyond expectability has happened in a given application scope. *)
exception Unexpected of string

(** Pretty prints known exceptions in a more fancy way. *)
let pretty_exn = function
    Unix.Unix_error (err, _, _) -> Unix.error_message err
  | Failure s                   -> s
  | e                           -> Printexc.to_string e
  

(** The type of typed marshalled data. *)
type 'a marshalled = Marshalled of string

(** Marshals any data (excluding closures and objects) to string. This operation is not typesafe. *)
let marsh x = Marshal.to_string x []

(** Unmarshals a string to any data (excluding closures and objects).
This operation is not typesafe and needs a type annotation (or unification in the type context by the type inference)
in order to return a concrete type.
*)
let unmarsh s = Marshal.from_string s 0

(** Marshals any data (excluding closures and objects) keeping the type information on the return type. *)
let polymarsh (x: 'a) : 'a marshalled = Marshalled (marsh x)

(** Unmarshals data marshalled with [polymarsh], resulting in a typed value. *)
let polyunmarsh (Marshalled s : 'a marshalled) : 'a = unmarsh s

(** Extracts the untyped data from a marshalled value. This operation makes the typesystem loose type information on the marshalled data and
is therefore unsafe.
*)
let string_of_marshalled (Marshalled s) = s


(** Raised when accessing a pointer referecing nothing. *)
exception Null_pointer

(** Pointer-like class. Optionally an initial [value] can be passed at construction; if not, the pointer initially references nothing.
*)
class ['a] pointer ?value () =
  object
  	(**/**)
  	val mutable pt : 'a option = value
  	
  	(** Dereferences the pointer. Raises [Null_pointer] if it references nothing. *)
  	method get = match pt with None -> raise Null_pointer | Some x -> x
  	
  	(** Sets the pointer to [x]. *)
  	method set x = pt <- Some x
  	
  	(** Queries whether the pointer is pointing to nothing. *)
  	method is_null = pt = None
  end	  
	  

(** Kilo multiplier for units. *)
let kilo n = 1024 * n

(** Mega multiplier for units. *)
let mega n = 1024 * kilo n

(** Giga multiplier for units. *)
let giga n = 1024 * mega n

(** Tera multiplier for units. *)
let tera n = 1024 * giga n

(** Peta multiplier for units. *)
let peta n = 1024 * tera n

(** Pretty prints a memory amount using the most appropriate unit and approximation.
[b] is the memory amount in bytes.
[magnitude_threshold] optionally sets the magnitude threshold at which switching to a higher unit.
*)
let pretty_memory ?(magnitude_threshold = 10.) b =
    let rec div (n, m) = let n' = n /. 1024. in if n' < magnitude_threshold or m = 4 then (n, m) else div (n', m + 1) in
    let (n, m) = div (b, 0) in
    let pretty_magnitude = function
        0 -> "B"
      | 1 -> "KB"
      | 2 -> "MB"
      | 3 -> "GB"
      | _ -> "TB"
    in
        Printf.sprintf "%.0f %s" n (pretty_magnitude m)
			
			(** Generates a random integer within the range [(a, b)] (including boundaries). *)
let rnd_int (a, b) = Random.int (b - a + 1) + a

(** Generates a random int32 within the range [(a, b)] (including boundaries). *)
let rnd_int32 (a, b) = Int32.add (Random.int32 (Int32.sub b (Int32.add a Int32.one))) a

(** Generates a random float within the range [(a, b)] (including [a], excluding [b]). *)
let rnd_float (a, b) = Random.float (b -. a) +. a


let rnd_nth = function
    [] -> raise (Invalid_argument "rnd_nth")
  | l  -> List.nth l (Random.int (List.length l))			

(** Swaps element at position [i] with element at position [j] in array [a]. *)			
let swap_array_elem a i j = 
  	let tmp = Array.get a i
  	in 
  		Array.set a i (Array.get a j);
   		Array.set a j tmp

(** Shuffles array [a] randomly. *)
let shuffle_array a = 
  	let n = Array.length a
  	in
    	for i = 0 to (n - 1) do 
      		swap_array_elem a i (i + (Random.int (n - i)));
    	done

(** Shuffles list [l] randomly. *)
let shuffle l =
	let a = Array.of_list l in
  	let () = shuffle_array a
  	in
  		Array.to_list a

(** Extracts elements from position [a] to [b] (including). *)
let range (a, b) arr = 
	let rec f acc = function
		0	-> acc
	  | n	-> f (arr.(a + n - 1) :: acc) (n - 1)
	in
		if a < 0 or a > b or b >= (Array.length arr) then raise (Invalid_argument "Prelude.range");
		f [] (b - a + 1)			

  
(** Evaluates [f x] if the [option] value is [Some x], [()] otherwise. *)
let on_some f = function Some x -> f x | None -> ()

(** Evaluates [Some (f x)] if the [option] value is [Some x], [None] otherwise. *)
let map_some f = function Some x -> Some (f x) | None -> None

(** Evaluates [x] if the [option] value is [Some x], [def] otherwise. *)
let something def = function None -> def | Some x -> x

(** Evaluates [true] if the [option] value if [Some], [false] otherwise. *)
let is_some = function None -> false | Some _ -> true

(** Logic negation of [is_some]. *)
let is_none o = not (is_some o)

(** Extracts [x] from an [option] value if it is [Some x], raises [Unexpected] if [None]. *)
let surely_some = function None -> raise (Unexpected "surely_some") | Some x -> x

(** Pretty option *)
let pretty_option f = function None -> "None" | Some x -> sprintf "Some %s" (f x)


  				
(* misc *)

(** Function for forcing an unit expression in an unit type context. *)
let void () = ()

(** Typical either type. *)
type ('a, 'b) either = Former of 'a | Latter of 'b

(** Simple tree type. *)
type 'a tree = Leaf of 'a | Tree of 'a tree list

(** Maps a tree recursively applying [f] to all nodes. *)
let rec map_tree f = function
	Leaf x -> Leaf (f x)
  | Tree l -> Tree (List.map (map_tree f) l)

(** Polymorphic idendity function. *)
let ident = fun x -> x

(** Evaluates [true] if [a = b] and [b = c]. *)
let eq3 a b c = a = b && b = c

(** Evaluates [true] if [a = b] and [b = c] and [c = d]. *)
let eq4 a b c d = a = b && b = c && c = d

(** Evaluates [true] if [a = b] and [b = c] and [c = d] and [d = e]. *)
let eq5 a b c d e = a = b && b = c && c = d && d = e

(** Evaluates a pair whose elements are the sum of the two given pairs elements. *)
let pair_add a b = (fst a + fst b, snd a + snd b)

(** Pops [n] elements from queue [q]. *)
let dequeue n q =
	let rec f n l =
		if n = 0 || Queue.is_empty q then l
		else let x = Queue.pop q in f (n - 1) (x :: l)
	in
		f n []
	
(** Converts an integer into a character with that ASCII code. *)	
let char_of_int = Char.chr

(** Crops [x] within the range [(a, b)]. *)
let crop (a, b) x = if x < a then a else if x > b then b else x

(** Evaluates [f z] until it is done.
[f] shall return either [`Done x] for stopping the retry - [x] will be the final result then - or [`Retry (timeout, z)], indicating to
retry again after [timeout] seconds and propagating the new data in [z].
*)
let rec retry z f =
	match f z with
		`Done x             -> x
	  | `Retry (timeout, z) -> Thread.delay timeout; retry z f
	  
(** Evaluates [f z] for either up to [times] times or until done.
[f]shall return either [`Done x] for stopping the retry - [x] will be the final result then - or [`Retry (timeout, z)], indicating to
retry again after [timeout] seconds and propagating the new data in [z].
After [times] times [finally z] will be returned.
*)	  
let retry_times ~times ~finally z f =
	let times = ref times in
	let f' z = if !times > 0 then (decr times; f z) else `Done (finally z)
	in
		retry z f'
(** Evaluates [f x] for [x] that starts from [x0] and ends at [x1] step [dx]. *)
let rec ffor f (x0, x1) dx =
	if x0 < x1 then begin
		f x0;
		ffor f (x0 +. dx, x1) dx
	  end
	else ()

(** Little module with utilities for pairs. *)
module Pair =
  struct
    (** Sorts a pair given a [compare] function. *)
    let sort compare (a, b) = if compare a b = -1 then (a, b) else (b, a)
    
    (** Compares the first elements of two pairs. *)
    let comparefst (a, _) (b, _) = compare a b
  end

(** Evaluates [tryf] and, if an exception [e] occurs, evaluates [finally] and then [on_exn e]; else evaluates [finally] anyway and returns
the return value of the evaluation of [tryf].
*)


let try_finally ?(on_exn = raise) tryf finally =
	let finally_done = ref false in
    let r =
		try tryf () 
		with e -> finally (); finally_done := true; on_exn e in
    if (not !finally_done) then finally ();
    r

(** Evaluates [body x] where [x] is the result of the evaluation of [constr];
if an exception [e] occurs during the evaluation of [constr], then returns [on_constr_exn e];
if an exception [e] occurs during the evaluation of [body x], then evaluates [destr x] and returns [on_body_exn e];
else evaluates [destr x] anyway and returns the return value of the evaluation of [body x].
*)
let manage_resource ?(on_constr_exn = raise) ?(on_body_exn = raise) constr body destr =
    try
        let x = constr ()
        in
            try_finally ~on_exn:on_body_exn (fun () -> body x) (fun () -> try destr x with _ -> ())
    with e -> on_constr_exn e

(**/**)
let __counter = ref 0
let __mutex = Mutex.create ()
(**/**)

(** Generates a fresh number atomically. *)
let fresh () =
	Mutex.lock __mutex;
	let r = !__counter in
	incr __counter;
	Mutex.unlock __mutex;
	r


(** Signature for defining the logical operation on the layout of a scalar type.
*)		
module type NumDumpType =
  sig
  	type t
  	val size 	  	  : int
  	val logic_and 	  : t -> t -> t
  	val logic_or  	  : t -> t -> t
  	val logic_shift_l : t -> int -> t
  	val logic_shift_r : t -> int -> t
  	val to_int		  : t -> int
  	val of_int		  : int -> t
  end			
					
(** Creates two conversion function (from e to string) for the given scalar type definition. Such functions can be used for dumping scalar
values to a string (either in big or little endian notation) and retrieve them back.
*)					
module NumDump (X : NumDumpType) =
  struct
	let to_string endian n =
		let s = String.create X.size
		and mask = X.of_int 0xff
		in
			for i = 0 to X.size - 1 do
				let b = X.logic_and (X.logic_shift_r n ((match endian with `Big_endian -> X.size - 1 - i | `Little_endian -> i) * 8)) mask in
				s.[i] <- Char.chr (X.to_int b)
			done;
			s
			
	let of_string endian s =
		let rec f i n =
			if i = X.size then n
			else f (i + 1) (X.logic_or n (X.logic_shift_l (X.of_int (Char.code s.[i]))
			        ((match endian with `Big_endian -> X.size - 1 - i | `Little_endian -> i) * 8)))
		in
			f 1 (X.of_int (Char.code s.[0]))
  end		
		
