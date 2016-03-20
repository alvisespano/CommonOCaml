(*
 * Common library
 * device.ml: abstract storage device types
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Abstract storage device types. Devices are storage units where operation such as loading, saving a deleting data is possibile in a
filesystem-like fashion.
*) 
 
open Prelude

(** Raised by all device implementations on failure. *)	
exception Failure of string * string

(** Class type for the readable/writable device. This device supports load, save and deletion of data through a key type. Additional operations
are existance query or a given key, flush and clean of the device.
*)
class type ['key, 'data] rw =
  object
	method load   : 'key -> 'data
	method save   : 'key -> 'data -> unit
	method exists : 'key -> bool
	method size	  : 'key -> int 
 	method flush  : unit
 	method delete : 'key -> unit
 	method clean  : unit
  end
      		     
(** Class type for the readable/writable/removable device. Refinement of the [rw] device type with a [poll] method.
*)
class type ['key, 'data] rwr =
  object inherit ['key, 'data] rw
 	method poll : bool
  end
      		       		 
      		 
