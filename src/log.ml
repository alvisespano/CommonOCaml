(*
 * Common library
 * log.ml: abstract log types
 *
 * (C) 2005 Quendi srl.
 *)

(** Abstract log types. *)

(** Message severity for log line types. *)
type severity = Debug | Info | Warn | Error | Fatal_error | Unmaskerable

(** Converts a message severity to string. *)
let string_of_severity = function
	Debug				-> "Debug"
  | Info				-> "Info"
  | Warn				-> "Warn"
  | Error				-> "Error"
  | Fatal_error			-> "Fatal_error"
  | Unmaskerable		-> "Unmaskerable"

(** Converts a string to severity. *)
let severity_of_string = function
	"Debug"				-> Debug
  | "Info"				-> Info
  | "Warn"				-> Warn
  | "Error"				-> Error
  | "Fatal_error"		-> Fatal_error
  | "Unmaskerable"		-> Unmaskerable
  | _					-> raise (Failure "Log.msg_kind_of_string")


let int_of_severity = function
	Debug			-> 0
  | Info			-> 1
  | Warn			-> 2
  | Error			-> 3
  | Fatal_error 	-> 4
  | Unmaskerable	-> 5
  

let compare_severity sev1 sev2 = compare (int_of_severity sev1) (int_of_severity sev2)


type history_log_line = float * severity * int list option * string * string

(** Logger class type. A logger is an object capable of offering the basic logging facilities. *)
class type logger =
  object
    (** Prompt a debug message. *)
  	method debug            : string -> unit

    (** Prompt an informational message. *)
    method info             : string -> unit
    
    (** Prompt a warning message. *)
   	method warn             : code:(int list) -> string -> unit

    (** Prompt an error message. *)
	method error			: code:(int list) -> string -> unit
    
    (** Prompt a fatal error message. *)
    method fatal_error      : code:(int list) -> string -> unit

    (** Prompt an unmaskerable message. *)
	method unmaskerable 	: string -> unit
       
    (** Sets level threshold. *)
    method set_threshold    : severity -> unit
  
    (** Gets current level threshold. *)
    method get_threshold    : severity

	(** Get latest messages *)
	 method history : severity option -> int option -> history_log_line list option

  end 




