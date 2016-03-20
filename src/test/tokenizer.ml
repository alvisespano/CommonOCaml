

open Printf
open Prelude

let logger = new LogLib.console_logger ()

let main filename = 
	let ich = open_in_bin filename in 
	let rec f () =
	  try 
		let line = input_line ich in
		let stream = XStream.tokenize ~sep:';' ~quote:'\"' line in
		let l = XStream.list_of_stream stream 
		in 
			logger#msg Log.Normal (sprintf "%s" (XString.flatten_strings ~sep:"---" l));
			f ()
		
	  with 
		End_of_file -> ()
	  | e			-> logger#warn Log.Normal (sprintf "exception caught: %s" (pretty_exn e)); f ()
	in
		f ()
	



;;

main Sys.argv.(1)

