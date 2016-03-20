(*
 * Lordaeron
 * Components
 * env.ml: pure environments
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Pure polymorphic environment. *) 
 
open Prelude
open Printf
open XList
 
(**/**)
module M = Map.Make (struct type t = string let compare = compare end) 
 
(** Polymorphic heavyweight environment time. *)
type 'a t = Env of 'a M.t

(** The empty environment. *)
let empty = Env M.empty

(** Binds a value [x] to symbol [id] on the environment. *)
let bind (Env m) id x = Env (M.add id x m)

(** Binds the list of values stored as second component of the list of pairs [l] to symbol stored as first component. *)
let bindl (Env m) l = Env (List.fold_left (fun m (id, x) -> M.add id x m) m l)

(** Looks up symbol [id] on the environment. *)
let lookup (Env m) id = try Some (M.find id m) with Not_found -> None

(** Checks whether symbol [id] exists in the environment. *)
let occurs (Env m) id = M.mem id m

(** Compose two environments, appending all the bindings of the second in the first. *)
let compose (Env m1) (Env m2) = Env (M.fold (fun id x env -> M.add id x env) m2 m1)

(** Pretty print an environment given a pretty printer [p] for the element type. *)
let pretty p (Env m) =
	let f (id, x) = sprintf "%s = %s" id (p x)
	in
		sprintf "{ %s }" (XString.mappen_strings f ~sep:"; " (M.fold (fun id x l -> (id, x) :: l) m []))
