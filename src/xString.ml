(*
 * Common library
 * xString.ml: extended strings
 *
 * (C) 2005 Quendi srl.
 *)

(** Stdlib String module extension. *)

open Printf
open Big_int
open Prelude

(** Flattens a list of strings given separator [sep]. The separator will not occur after the last element but only between elements. *)
let flatten_strings ?(sep = " ") l =
	let rec f r = function
        []      -> r
      | s :: [] -> r ^ s
      | s :: l  -> f (r ^ s ^ sep) l
	in
		f "" l
          
(** Flattens an array of strings given separator [sep]. The separator will not occur after the last element but only between elements. *)          
let flatten_string_array argv = flatten_strings (Array.to_list argv)

(** Flattens a list of value of any type mapping them to string through function [f]. [sep] is the separator between elements. 
The separator will not occur after the last element but only between elements.
*)
let mappen_strings f ?(sep = " ") l = flatten_strings ~sep (List.map f l)

(** Converts a char into a string. *)	
let string_of_char c = String.make 1 c	(* non sognarsi di usare Char.escape!! *)

(** Converts a string into a char picking its first element. Equivalent to [String.get s 0]. *)
let char_of_string s = String.get s 0

(** Converts an array of chars into a string. *)
let string_of_char_array ca =
	let buf = Buffer.create (Array.length ca)
	in
		Array.iter (Buffer.add_char buf) ca;
		Buffer.contents buf
		
(** Converts a list of chars into a string. *)		
let string_of_chars cs = string_of_char_array (Array.of_list cs)

(** Truncates string [s] up to length [len] suffixing [suffix] if it has actually been truncated. *)
let truncate_string ?(suffix = "...") s len =
	if String.length s <= len then s
	else String.sub s 0 len ^ suffix

(** Escapes all occurences of characters in [chars] in string [s] using [escaper] as escape character.
Escaped characters are of the form [Exyz] where [E] is the escape character and [xyz] are 3 decimal digits specifying the ASCII code of the char.
The escaper char will be automatically escaped as well.
*)
let escape ?(escaper = '\\') chars s =
	let chars = List.filter (fun c -> c <> escaper) chars in
	let f s c =	Str.global_substitute (Str.regexp_string (string_of_char c))
					(fun _ -> sprintf "%c%03d" escaper (Char.code c)) s
	in
		List.fold_left f s (escaper :: chars)

(** Unescapes all occurrences of characters in [chars] in string [s] using [escaper] as escape character.
The escaping format is compatible with the [escape] function above.
The escaper char will be automatically unescaped as well.
*)
let unescape ?(escaper = '\\') chars s =
	let chars = List.filter (fun c -> c <> escaper) chars in
	let f c s =	Str.global_substitute (Str.regexp_string (sprintf "%c%03d" escaper (Char.code c)))
					(fun _ -> string_of_char c) s
	in
		List.fold_right f (escaper :: chars) s

(** Removes all occurences of char [c] at the beginning or at the end of string [s]. *)
let trim c s = 
	try
		let ss = Str.search_forward (Str.regexp (sprintf "[^%c]+" c)) s 0 
		and ee = Str.search_backward (Str.regexp (sprintf "[^%c]+" c)) s (String.length s) in
		let sub1 = Str.string_after s ss
		in
			(Str.string_before sub1 (ee + 1 - ss))
	with _ -> ""
		
(** Removes all occurences of the whitespace character at the beginning or at the end of string [s]. *)	
let trim_spaces = trim ' '

(** Creates an alphanumeric string given any string [s]. Useful for remangling any string into a printable string. *)
let printable s =
	let (++) = add_big_int in
	let exp = power_int_positive_int in
	let ( ** ) = mult_int_big_int in
	let a = Array.init (String.length s) (fun i -> Char.code (s.[i])) in
	let n256 =
		let f c (n256, i) = (n256 ++ c ** (exp 256 i), i + 1)
		in
			fst (Array.fold_right f a (zero_big_int, 0))
	in
	let rec f n256 =
		let (q, r) = quomod_big_int n256 (big_int_of_int 36) in
		let r = int_of_big_int r in
		let c = Char.chr (if r <= 9 then Char.code '0' + r else Char.code 'a' + (r - 10))
		in
			string_of_char c ^ (if gt_big_int q zero_big_int then f q else "")
	in
		f n256
		
(** Splits string [s] in as many parts as necessary where each part length is [n] (possibly with the exception of the last one). *)
let split n s =
    let rec f a = if a >= String.length s then [] else let b = crop (0, String.length s) (a + n) in (String.sub s a (b - a)) :: (f b)
    in
        if n > 0 then f 0 else raise (Invalid_argument (sprintf "XString.split: n (%d) <= 0" n))

(** Creates a list of strings given the list of pairs of strings [cols]. The pairs represent two columns of strings to be rendered properly.
[left_width] is the width (in characters) of the left column - strings longer than it will be truncated.
[min_pad] is the minimum number of pad characters for separating the two columns.
[dot] is the pad character for separating the two columns.
[newline_dot] is the pad character used to re-indent the second column when it is too long and a newline is needed (keep in mind that the second 
column will therefore never be truncated).
[line_width] is the overall line width (in characters).
*)
let render_columns2 ~left_width ~min_pad ~dot ~newline_dot ~line_width cols =
        if left_width + min_pad >= line_width then raise (Invalid_argument (sprintf "render_columns2: left_width (%d) + min_pad (%d) >= line_width (%d)" left_width min_pad line_width));
        let (longest_a, _) = surely_some (XList.find_max (fun (a, _) (a', _) -> compare (String.length a) (String.length a')) cols) in
		let tablen = (crop (0, left_width) (String.length longest_a)) + min_pad in
        let f (a, b) lines =
            let a = String.sub a 0 (min (String.length a) left_width) in
	        let dots = String.make (tablen - (String.length a)) dot
	        and tab = String.make tablen newline_dot
	        in
	            match split (line_width - tablen) b with
	                []      -> a :: lines
	              | h :: t  -> (sprintf "%s%s%s" a dots h) :: (List.map (fun s -> tab ^ s) t) @ lines
		in
		    List.fold_right f cols []

(** Generates a string made of random decimal digits. *)
let fresh_digits n = string_of_char_array (Array.init n (fun _ -> Char.chr (48 + Random.int 9)))

(** Checks whether a string [s] begins with [needle]. *)
let starts_with needle s = Str.string_match (Str.regexp (sprintf "^%s" needle)) s 0 

(** Checks whether a string [s] ends with [needle]. *)
let ends_with needle s = 
	try 
		let len = String.length needle 	
		in
			String.compare needle (String.sub s (String.length s - len) len) = 0
	with _ -> false


(**/**) (* todo: commentare subst *)

let subst ?(prefix = "$") ?(suffix = "$") s subs =
    List.fold_left (fun s (v, x) -> Str.global_substitute (Str.regexp_string (sprintf "%s%s%s" prefix v suffix)) (fun _ -> x) s ) s subs

(**/**)

