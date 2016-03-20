open Printf
open Prelude

let logger = new LogLib.color_console_logger ()

let size = 2

let dynbuffer () = 
	let data : string Containers.dynbuffer = new Containers.dynbuffer size "--" in
	logger#msg Log.Normal (sprintf "dynbuffer created with array size %d" size);
	let usage = "a <s>: append s; g <n>: get n; l: length;" in
	logger#msg Log.High usage;
	while true do 
	  try
		let str = XStream.tokenize ~sep:' ' ~quote:'\'' (input_line stdin) in 
		let f = parser  
			[< '"a"; 's >]	-> data#append s
		  | [< '"g"; 'n >]	-> let n = int_of_string n in logger#msg Log.Normal (sprintf "%d: %s" n (data#get n))
		  | [< '"l" >]		-> logger#msg Log.Normal (sprintf "%d" data#length)		  
		  | [< _ >]			-> logger#msg Log.Normal usage
		in
			f str
	  with e -> logger#warn Log.Normal (pretty_exn e)
	done


;;

dynbuffer ()
