(*
 * Angband
 * Common library
 * externalConfigAbsyn.ml: external configuration file abstract syntax tree
 *
 * (C) 2005 Quendi srl.
 *)
 
type value = Float of float
  	       | Int of int
  		   | String of string
  		   | Char of char 
  		   | Bool of bool
  		   | List of value list
		   | Tuple of value list
		   | Record of record
		   | File of string
		   | EnumId of string
 
and binding = string * value

and record = binding list

and enum = string * string list

and file = enum list * record

module Record = Map.Make (struct type t = string let compare = compare end)

module EnumSet = Set.Make (struct type t = string let compare = compare end)

type ty = TyFloat
		| TyInt
		| TyString
		| TyChar
		| TyBool
		| TyList of ty
		| TyTuple of ty list
		| TyRecord of ty Record.t
		| TyEnum of EnumSet.t


