
open Printf 
open Prelude

exception Parse_error of string 

let logger = new LogLib.console_logger ()

let pretty_datetime = Time.pretty_datetime 
								~date_notation:`YYYYMMDD 
								~date_sep:"-" 
								~timeofday_sep:":" 
								~pretty_weekday:None 
								~subsecond_digits:3 

let datetime_of_string s = 
  try
	match Str.split (Str.regexp "[ :\\.-]+") s with 
		year :: month :: day :: hours :: mins :: secs :: subsecs :: [] -> 
			(Time.set_time 
					~year:(int_of_string year)
					~month:(int_of_string month)
					~day:(int_of_string day)
					~hours:(int_of_string hours)
					~mins:(int_of_string mins)
					~secs:(int_of_string secs)
					0.0) +. (float_of_int (int_of_string subsecs) /. 1000.0)
	  | _ -> raise (Parse_error "datetime_of_string")
  with e -> raise (Parse_error (sprintf "datetime_of_string: %s" (pretty_exn e)))

let main () = 
	let t = Time.now () in
	let t' = datetime_of_string (pretty_datetime t)
	in
		logger#msg Log.Normal (sprintf "%s :: %s" (pretty_datetime t) (pretty_datetime t'))

;;

main ()
		
