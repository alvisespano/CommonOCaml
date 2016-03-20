(*
 * Common library
 * time.ml: time facilities and Unix time interface
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Time facilities and Unix time interface. The timestamp type is a float normalized to seconds as for Unix. *)
 
open Printf
open Prelude

 
(** Weekday type. *)  
type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

(** Creates a float timestamp given an integer in seconds. *)
let secs n = float_of_int n

(** Creates a float timestamp given an integer in minutes. *)
let minutes n = secs (60 * n)

(** Creates a float timestamp given an integer in hours. *)
let hours n = minutes (60 * n)

(** Creates a float timestamp given an integer in days. *)
let days n = hours (24 * n)

(** Converts a Unix weekday to weekday. *)
let weekday_of_unix_wday = function
	0 -> Sun
  | 1 -> Mon
  | 2 -> Tue
  | 3 -> Wed
  | 4 -> Thu
  | 5 -> Fri
  | 6 -> Sat
  | n -> raise (Unexpected (sprintf "weekday_of_unix_wday: invalid wday %d" n))
  
(** Pretty prints weekday. *)  
let pretty_weekday = function
	Mon -> "Mon"
  | Tue -> "Tue"
  | Wed -> "Wed"
  | Thu -> "Thu"
  | Fri -> "Fri"
  | Sat -> "Sat"
  | Sun -> "Sun"
  
  
(** Get the current timestamp. The value is normalized to seconds but is nore precise than a second. *)  
let now = Unix.gettimeofday

(** Alias for Unix.localtime. *)
let tm = Unix.localtime 

(** Measures the amount of time elapsed by the evaluation of [f]. *)
let elapsed f =
    let a = now ()
    and x = f ()
    and b = now ()
    in
        (x, b -. a)

(** Convert [tm] back to a float timestamp. *)
let to_timestamp tm = fst (Unix.mktime tm)

(** Gets the current timezone. *)
let timezone () =
	let g = Unix.time () in
	let l = fst (Unix.mktime (Unix.localtime g))
	in
		l -. g

(** Maps [x] changing only those fields specified by the optional parameters. *)		
let set_tm ?year ?month ?day ?hours ?mins ?secs x =
	{ Unix.tm_sec = something x.Unix.tm_sec secs;
	  Unix.tm_min = something x.Unix.tm_min mins;
	  Unix.tm_hour = something x.Unix.tm_hour hours;
	  Unix.tm_mday = something x.Unix.tm_mday day;
	  Unix.tm_mon = (something (x.Unix.tm_mon + 1) month) - 1;
	  Unix.tm_year = (something (x.Unix.tm_year + 1900) year) - 1900;
	  Unix.tm_wday = 0;
	  Unix.tm_yday = 0;
	  Unix.tm_isdst = false }
	  
(** Maps [x] changing only those fields specified by the optional parameters. *)
let set_time ?year ?month ?day ?hours ?mins ?secs x =
	to_timestamp (set_tm ?year ?month ?day ?hours ?mins ?secs (tm x))   
	
(** Adds those fields specified by the optional parameters to [x]. *)	  
let add_tm ?days ?hours ?mins ?secs x =
	{ Unix.tm_sec = something x.Unix.tm_sec (map_some (fun t -> x.Unix.tm_sec + t) secs);
	  Unix.tm_min = something x.Unix.tm_min (map_some (fun t -> x.Unix.tm_min + t) mins);
	  Unix.tm_hour = something x.Unix.tm_hour (map_some (fun t -> x.Unix.tm_hour + t) hours);
	  Unix.tm_mday = something x.Unix.tm_mday (map_some (fun t -> x.Unix.tm_mday + t) days);
	  Unix.tm_mon = x.Unix.tm_mon;
	  Unix.tm_year = x.Unix.tm_year;
	  Unix.tm_wday = 0;
	  Unix.tm_yday = 0;
	  Unix.tm_isdst = false }

(** Adds those fields specified by the optional parameters to [x]. *)	  
let add_time ?days ?hours ?mins ?secs x = 
	to_timestamp (add_tm ?days ?hours ?mins ?secs (tm x))

(** Verifies whether year [y] is a leap year. *)
let is_leap_year y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0
	
(** Crop month day [d] within month [m] in year [y]. *)
let crop_monthday y m d =
	match m with
		4 | 6 | 9 | 11 -> crop (1, 30) d
	  | 2			   -> crop (1, if is_leap_year y then 29 else 28) d
	  | _			   -> crop (1, 31) d
	
  
(** Interval type for relative time. Stands for the quadruple (days, hours, minutes, seconds). *)
type interval = Interval of (int * int * int * int)

(** Converts an interval to a timestamp. *)
let float_of_interval (Interval (d, h, m, s)) = days d +. hours h +. minutes m +. secs s

(** Converts a timestamp to an interval. *)
let interval_of_float t = 			
	let (days, rem) = (t /. days 1, mod_float t (days 1)) in
	let (hours, rem) = (rem /. (hours 1), mod_float rem (hours 1)) in
	let (minutes, seconds) = (rem /. (minutes 1), mod_float rem (minutes 1))
	in
		Interval (int_of_float days, int_of_float hours, int_of_float minutes, int_of_float seconds)
		
(** Converts a string to an interval. *)		
let interval_of_string s =
	Interval (try Scanf.sscanf s "%d.%d:%d:%d" (fun d h m s -> (d, h, m, s))
	          with _ -> try Scanf.sscanf s "%d:%d:%d" (fun h m s -> (0, h, m, s))
			            with _ -> try Scanf.sscanf s "%d:%d" (fun m s -> (0, 0, m, s))
					              with _ -> Scanf.sscanf s ":%d" (fun s -> (0, 0, 0, s)))
			
(** Pretty prints an interval. *)				
let pretty_interval (Interval (d, h, m, s)) = 
	sprintf "%s%s%s%d secs" 
		(if d > 0 then sprintf "%d days, " d else "")
		(if h > 0 then sprintf "%d hours, " h else "")
		(if m > 0 then sprintf "%d mins, " m else "")
		s

(** Pretty prints a date.
[sep] optionally sets the separator.
[notation] is one of [`DDMMYYYY], [`MMDDYYYY] or [`YYYYMMDD] and specifies the output format for the date.
[pretty_weekday] is the function for optionally pretty printing the weekday.
[x] is the timestamp.
*)  
let pretty_date ?(sep = "/") ?(notation = `DDMMYYYY) ?(pretty_weekday = Some pretty_weekday) x = 
	let tm = Unix.localtime x in
	let (a, b, c) =
	    let m = sprintf "%02d" (tm.Unix.tm_mon + 1)
	    and d = sprintf "%02d" tm.Unix.tm_mday
	    and y = sprintf "%04d" (tm.Unix.tm_year + 1900)
	    in
	        match notation with
		        `DDMMYYYY -> (d, m, y)
		      | `MMDDYYYY -> (m, d, y)
		      | `YYYYMMDD -> (y, m, d)
	in
		sprintf "%s%s%s%s%s%s"
		    (match pretty_weekday with None -> "" | Some f -> f (weekday_of_unix_wday tm.Unix.tm_wday) ^ " ")
			a sep b sep c

(** Pretty prints the time of day.
[sep] is the separator.
[subseconds_digits] sets the number of digits for fractional seconds.
[x] is the timestamp.
*)
let pretty_timeofday ?(sep = ":") ?(subsecond_digits = 0) x =
	let tm = Unix.localtime x in
	let sub =
	    if subsecond_digits > 0 then
	        let m = 10.0 ** (float_of_int subsecond_digits) in
	        let sub = sprintf "%d" (int_of_float (Math.fmod (x *. m) m)) in
	        let pad = String.make (subsecond_digits - (String.length sub)) '0'
	        in
	            sprintf ".%s%s" pad sub
	    else ""
	in
		sprintf "%02d%s%02d%s%02d%s" tm.Unix.tm_hour sep tm.Unix.tm_min sep tm.Unix.tm_sec sub

(** Pretty print both the date and the time of day.
[date_notation] is one of [`DDMMYYYY], [`MMDDYYYY] or [`YYYYMMDD] and specifies the output format for the date.
[date_sep] is the separator for the date.
[timeofday_sep] is the separator for the timeofday.
[pretty_weekday] is the function for optionally pretty printing the weekday.
[subseconds_digits] sets the number of digits for fractional seconds.
[x] is the timestamp.
*)
let pretty_datetime ?date_notation ?date_sep ?timeofday_sep ?pretty_weekday ?subsecond_digits x =
    sprintf "%s %s" (pretty_date ?notation:date_notation ?pretty_weekday ?sep:date_sep x)
                    (pretty_timeofday ?sep:timeofday_sep ?subsecond_digits x)
	
		
	
	


