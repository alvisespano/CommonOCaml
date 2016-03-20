(*
 * ExternalConfig
 * externalConfig.ml: external configuration files
 *
 * (C) 2005-2006 Alvise Spano'
 *)
 
open Prelude
open ExternalConfigAbsyn
open Printf

exception Syntax_error of string
exception Type_error of string
exception Id_not_found of string
exception Type_mismatch of string * value
exception File_error of string
 

(* misc *)  

let env_of_record r =
	let f env (id, v) = Env.bind env id v
	in
		List.fold_left f Env.empty r

let parse p lexbuf =
	Parsing.clear_parser ();
	ExternalConfigLexer.clear_lexer ();
	try p ExternalConfigLexer.token lexbuf
	with Failure s   		 -> raise (Syntax_error (sprintf "syntax error at line %d: %s" !ExternalConfigLexer.line_count s))
   	  |  Parsing.Parse_error -> raise (Syntax_error (sprintf "syntax error at line %d" !ExternalConfigLexer.line_count))

let lookup env id =	match Env.lookup env id with Some v -> v | None -> raise (Id_not_found id)
   	  
let load_file filename =
	try Io.rlock_file filename 0 (fun fd ->	
	  		let lexbuf = Lexing.from_channel (Unix.in_channel_of_descr fd)
			in
				parse ExternalConfigParser.file lexbuf)
	with Syntax_error s -> raise (Syntax_error (sprintf "%s: %s" filename s))
	  |  e 				-> raise (File_error (sprintf "%s: %s" filename (pretty_exn e)))
	  
let repath fatherpath childpath =
	if Filename.is_relative childpath then Filename.concat (Filename.dirname fatherpath) childpath
	else childpath
	  
let absolute_path path =
	if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
	  

(* type checking *)

let rec ty_match ty1 ty2 =
	match (ty1, ty2) with
		(TyRecord r1, TyRecord r2) 	-> Record.equal ty_match r1 r2
		
	  | (TyList ty1, TyList ty2)   	-> ty_match ty1 ty2
	  
	  | (TyTuple tys1, TyTuple tys2)-> (try List.for_all2 ty_match tys1 tys2
	  								   with Invalid_argument _ -> raise (Type_error "tuples are not isomorph"))
									   
	  | (TyEnum e1, TyEnum e2) 		-> EnumSet.equal e1 e2
	  
	  |	(TyFloat, TyFloat)
	  | (TyInt, TyInt)
	  | (TyString, TyString)
	  | (TyBool, TyBool)
	  | (TyChar, TyChar)			-> true
	  
	  | _							-> false


let rec type_check_record abspath_history fatherpath enums bs =
	List.fold_left (fun map (id, v) ->
		if Record.mem id map then raise (Type_error (sprintf "identifier %s already defined in record" id))
		else Record.add id (type_check_value abspath_history fatherpath enums v) map) Record.empty bs

and type_check_file abspath_history fatherpath (enums, r) =
	let rec check_enums acc = function
		[] 				  -> ()
	  | (id, es) :: enums ->
	  		match XList.intersect acc es with
	  			[] -> check_enums (acc @ es) enums
			  | es -> raise (Type_error (sprintf "enumeration %s defines members %s which are already defined" id (XString.flatten_strings ~sep:", " es)))
	in
		check_enums [] enums;
		type_check_record abspath_history fatherpath enums r

and type_check_value abspath_history fatherpath enums = function
	Float _	 -> TyFloat
  | Int	_	 -> TyInt
  | String _ -> TyString
  | Char _	 -> TyChar
  | Bool _	 -> TyBool
  
  | List [] -> raise (Unexpected "ExternalConfig.type_check_value: empty list value")
  
  | List (v :: vs) ->
  		let ty = type_check_value abspath_history fatherpath enums v
		in
			if List.for_all (fun ty' -> ty_match ty ty') (List.map (type_check_value abspath_history fatherpath enums) vs) then TyList ty
			else raise (Type_error "list is not omogeneous")
  
  | Tuple vs -> TyTuple (List.map (type_check_value abspath_history fatherpath enums) vs)
  
  | Record bs -> TyRecord (type_check_record abspath_history fatherpath enums bs)
  
  | EnumId s ->
  		let module M = XList.SetOfList (EnumSet) in
		let (_, es) = try List.find (fun (_, es) -> XList.occurs s es) enums
					  with Not_found -> raise (Type_error (sprintf "undefined enumeration member %s" s))
		in
			TyEnum (M.f es)
			
  | File s ->
  		let path = repath fatherpath s in
		let abspath = absolute_path path
		in
			if XList.occurs abspath abspath_history then raise (Type_error (sprintf "recursive reference to file %S" abspath))
			else TyRecord (type_check_file (abspath :: abspath_history) path (load_file path))
  
 
let rec pretty_ty_record ~sep r =
	Record.fold (fun id ty s -> s ^ (sprintf "%s : %s%s" id (pretty_ty ty) sep)) r ""

and pretty_ty = function
	TyFloat 	-> "float"
  | TyInt 		-> "int"
  | TyString	-> "string"
  | TyChar 		-> "char"
  | TyBool		-> "bool"
  | TyList ty	-> sprintf "%s list" (pretty_ty ty)
  | TyTuple tys -> "(" ^ (XString.mappen_strings ~sep:" * " pretty_ty tys) ^ ")"
  | TyRecord bs -> "{ " ^ (pretty_ty_record ~sep:"; " bs) ^ " }"
  | TyEnum es   -> "[ " ^ (XString.flatten_strings ~sep:" | " (EnumSet.elements es)) ^ " ]"

	  
(* main classes *)

class type record_t =
  object
    val env : value Env.t XThread.mut
  
    method filename : string

	method lookup : string -> value
  
  	method lookup_int : string -> int
	method lookup_float : string -> float
	method lookup_char : string -> char
	method lookup_bool : string -> bool
	method lookup_string : string -> string
  	method lookup_record : string -> record_t
	method lookup_enum : 'a. string -> (string * 'a) array -> 'a 
	method lookup_list : 'a. (value -> 'a) -> string -> 'a list
	method lookup_array : 'a. (value -> 'a) -> string -> 'a array 
	method lookup_tuple2 : 'a 'b. ((value -> 'a) * (value -> 'b)) -> string -> 'a * 'b 
	method lookup_tuple3 : 'a 'b 'c. ((value -> 'a) * (value -> 'b) * (value -> 'c)) -> string -> 'a * 'b * 'c 
	method lookup_tuple4 : 'a 'b 'c 'd. ((value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd)) -> string -> 'a * 'b * 'c * 'd 
		  
	(* transformers *)

	method trans_int : value -> int
	method trans_float : value -> float
	method trans_char : value -> char
	method trans_bool : value -> bool
	method trans_string : value -> string
	method trans_enum : 'a. value -> (string * 'a) array -> 'a 
	method trans_list : 'a. (value -> 'a) -> value -> 'a list 
	method trans_array : 'a. (value -> 'a) -> value -> 'a array 
	method trans_record : value -> record_t
	method trans_tuple2 : 'a 'b. (value -> 'a) * (value -> 'b) -> value -> 'a * 'b 
	method trans_tuple3 : 'a 'b 'c. (value -> 'a) * (value -> 'b) * (value -> 'c) -> value -> 'a * 'b * 'c 
	method trans_tuple4 : 'a 'b 'c 'd. (value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd) -> value -> 'a * 'b * 'c * 'd
  end
	
class type file_t =
  object inherit record_t
    method reload : Log.logger -> unit
  end
	
class record : string -> ExternalConfigAbsyn.record -> record_t = fun fatherpath r ->
  object (self)
  	val env = new XThread.mut (env_of_record r)
  
  	method filename = fatherpath
  
  	(* lookups *)
  
  	method lookup id = lookup env#get id

	method lookup_int id = self#trans_int (self#lookup id)
	method lookup_float id = self#trans_float (self#lookup id)
	method lookup_char id = self#trans_char (self#lookup id)
	method lookup_bool id = self#trans_bool (self#lookup id)
	method lookup_string id = self#trans_string (self#lookup id)
  	method lookup_record id = self#trans_record (self#lookup id)
	
	method lookup_enum : 'a. string -> (string * 'a) array -> 'a = fun id ->
		self#trans_enum (self#lookup id)	
	
	method lookup_list : 'a. (value -> 'a) -> string -> 'a list = fun trans id ->
		self#trans_list trans (self#lookup id)
	
	method lookup_array : 'a. (value -> 'a) -> string -> 'a array = fun trans id ->
		self#trans_array trans (self#lookup id)
		
	method lookup_tuple2 : 'a 'b. ((value -> 'a) * (value -> 'b)) -> string -> 'a * 'b = fun trans id ->
		self#trans_tuple2 trans (self#lookup id)
		
	method lookup_tuple3 : 'a 'b 'c. ((value -> 'a) * (value -> 'b) * (value -> 'c)) -> string -> 'a * 'b * 'c = fun trans id ->
		self#trans_tuple3 trans (self#lookup id)
		
	method lookup_tuple4 : 'a 'b 'c 'd. ((value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd)) -> string -> 'a * 'b * 'c * 'd = fun trans id ->
		self#trans_tuple4 trans (self#lookup id)
		  
	(* transformers *)

	method trans_int = function Int n -> n | v -> raise (Type_mismatch ("int", v))
	method trans_float = function Float n -> n | v -> raise (Type_mismatch ("float", v))
	method trans_char = function Char n -> n | v -> raise (Type_mismatch ("char", v))
	method trans_bool = function Bool n -> n | v -> raise (Type_mismatch ("bool", v))
	method trans_string = function String s -> s | v -> raise (Type_mismatch ("string", v))
	
	method trans_enum : 'a. value -> (string * 'a) array -> 'a =
		 function EnumId s as v -> fun a -> (try let l = Array.to_list a in XList.assoc_fst s l
		   						    		 with Not_found -> raise (Type_mismatch (sprintf "%s does not belong to enumeration" s, v)))
		    |     v        		-> raise (Type_mismatch ("enumeration", v))
	
	method trans_list : 'a. (value -> 'a) -> value -> 'a list =
		fun trans -> function List vs -> List.map trans vs | v -> raise (Type_mismatch ("list", v))
		
	method trans_array : 'a. (value -> 'a) -> value -> 'a array =
		fun trans -> function List vs -> Array.of_list (List.map trans vs) | v -> raise (Type_mismatch ("array", v))

	method trans_record = function
		Record r -> new record fatherpath r
	  | File s   -> (new file (repath fatherpath s) :> record_t)
	  | v 		 -> raise (Type_mismatch ("record", v))
	  
	method trans_tuple2 : 'a 'b. (value -> 'a) * (value -> 'b) -> value -> 'a * 'b =
		fun (trans1, trans2) -> function
		   Tuple [a; b] -> (trans1 a, trans2 b)
		 | v 			-> raise (Type_mismatch ("tuple2", v))

	method trans_tuple3 : 'a 'b 'c. (value -> 'a) * (value -> 'b) * (value -> 'c) -> value -> 'a * 'b * 'c =
		fun (trans1, trans2, trans3) -> function
		   Tuple [a; b; c] -> (trans1 a, trans2 b, trans3 c)
		 | v 			   -> raise (Type_mismatch ("tuple3", v))
		 
	method trans_tuple4 : 'a 'b 'c 'd. (value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd) -> value -> 'a * 'b * 'c * 'd =
		fun (trans1, trans2, trans3, trans4) -> function
		   Tuple [a; b; c; d] -> (trans1 a, trans2 b, trans3 c, trans4 d)
		 | v 				  -> raise (Type_mismatch ("tuple4", v))
	 
  end

  
and file : string -> file_t = fun filename ->
  object (self) inherit record filename []
  	  	 	  
    val last_load = new XThread.sync 0.
  	  	 	  
	initializer self#load
		        
	method private load	=
	    let (_, r) as file = load_file filename
        in
            ignore (type_check_file [filename] filename file);
    	    env#set (env_of_record r);
		        
    method reload (logger : #Log.logger) =
        last_load#apply_and_set (fun last_load ->
    	    let stamp = try Io.get_modification_time filename with e -> raise (File_error (sprintf "%s: %s" filename (pretty_exn e)))
            in
                if last_load < stamp then begin
                    self#load;
		            logger#info (sprintf "external configuration file reloaded: %s" filename);
  		            Time.now ()
    		      end
    		    else last_load)
    		    
  end

let files filenames = 
	let load filename = 
	  try `File (new file filename) 
	  with e -> `Failure e
	in
		let rec fold exns = function
			[]						-> raise (File_error (XString.mappen_strings ~sep:"; " pretty_exn exns))
		  | filename :: filenames	-> 
			  begin
				match load filename with 
					`Failure e	-> fold (e :: exns) filenames
				  | `File xc	-> (xc, filename)
			  end
		in
			fold [] filenames
	



(* todo: implementare decentemente senza copia incolla -- analizzare efficienza *)

(*
let files filenames : file_t =
  	match XList.filter_some (List.map (fun filename -> try Some (new file filename) with _ -> None) filenames) with
		[]		-> raise (File_error "none of the specified files could be loaded")
	  | files	-> 
		  object (self)
			
			method lookup id = 
				let rec f = function
					[]				-> raise (File_error "no files specified")
				  | file :: []		-> file#lookup id
				  | file :: files	-> try file#lookup id with _ -> f files; 
						
				in
					f files  
  

			val fatherpath = "__fatherpath"
			method filename = "__filename"
			method reload logger = List.iter (fun file -> file#reload logger) files

			method lookup_int id = self#trans_int (self#lookup id)
			method lookup_float id = self#trans_float (self#lookup id)
			method lookup_char id = self#trans_char (self#lookup id)
			method lookup_bool id = self#trans_bool (self#lookup id)
			method lookup_string id = self#trans_string (self#lookup id)
		  	method lookup_record id = self#trans_record (self#lookup id)
			
			method lookup_enum : 'a. string -> (string * 'a) array -> 'a = fun id ->
				self#trans_enum (self#lookup id)	
			
			method lookup_list : 'a. (value -> 'a) -> string -> 'a list = fun trans id ->
				self#trans_list trans (self#lookup id)
			
			method lookup_array : 'a. (value -> 'a) -> string -> 'a array = fun trans id ->
				self#trans_array trans (self#lookup id)
				
			method lookup_tuple2 : 'a 'b. ((value -> 'a) * (value -> 'b)) -> string -> 'a * 'b = fun trans id ->
				self#trans_tuple2 trans (self#lookup id)
				
			method lookup_tuple3 : 'a 'b 'c. ((value -> 'a) * (value -> 'b) * (value -> 'c)) -> string -> 'a * 'b * 'c = fun trans id ->
				self#trans_tuple3 trans (self#lookup id)
				
			method lookup_tuple4 : 'a 'b 'c 'd. ((value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd)) -> string -> 'a * 'b * 'c * 'd = fun trans id ->
				self#trans_tuple4 trans (self#lookup id)
				  
			

			method trans_int = function Int n -> n | v -> raise (Type_mismatch ("int", v))
			method trans_float = function Float n -> n | v -> raise (Type_mismatch ("float", v))
			method trans_char = function Char n -> n | v -> raise (Type_mismatch ("char", v))
			method trans_bool = function Bool n -> n | v -> raise (Type_mismatch ("bool", v))
			method trans_string = function String s -> s | v -> raise (Type_mismatch ("string", v))
			
			method trans_enum : 'a. value -> (string * 'a) array -> 'a =
				 function EnumId s as v -> fun a -> (try let l = Array.to_list a in XList.assoc_fst s l
				   						    		 with Not_found -> raise (Type_mismatch (sprintf "%s does not belong to enumeration" s, v)))
				    |     v        		-> raise (Type_mismatch ("enumeration", v))
			
			method trans_list : 'a. (value -> 'a) -> value -> 'a list =
				fun trans -> function List vs -> List.map trans vs | v -> raise (Type_mismatch ("list", v))
				
			method trans_array : 'a. (value -> 'a) -> value -> 'a array =
				fun trans -> function List vs -> Array.of_list (List.map trans vs) | v -> raise (Type_mismatch ("array", v))

			method trans_record = function
				Record r -> new record fatherpath r
			  | File s   -> (new file (repath fatherpath s) :> record_t)
			  | v 		 -> raise (Type_mismatch ("record", v))
			  
			method trans_tuple2 : 'a 'b. (value -> 'a) * (value -> 'b) -> value -> 'a * 'b =
				fun (trans1, trans2) -> function
				   Tuple [a; b] -> (trans1 a, trans2 b)
				 | v 			-> raise (Type_mismatch ("tuple2", v))

			method trans_tuple3 : 'a 'b 'c. (value -> 'a) * (value -> 'b) * (value -> 'c) -> value -> 'a * 'b * 'c =
				fun (trans1, trans2, trans3) -> function
				   Tuple [a; b; c] -> (trans1 a, trans2 b, trans3 c)
				 | v 			   -> raise (Type_mismatch ("tuple3", v))
				 
			method trans_tuple4 : 'a 'b 'c 'd. (value -> 'a) * (value -> 'b) * (value -> 'c) * (value -> 'd) -> value -> 'a * 'b * 'c * 'd =
				fun (trans1, trans2, trans3, trans4) -> function
				   Tuple [a; b; c; d] -> (trans1 a, trans2 b, trans3 c, trans4 d)
				 | v 				  -> raise (Type_mismatch ("tuple4", v))

  end

*)

(*
let files filenames =
  	match XList.filter_some (List.map (fun filename -> try Some (new file filename) with _ -> None) filenames) with
		[]		-> raise (File_error "none of the specified files could be loaded")
	  | files	-> 
		  object inherit file ""
			
			method lookup id = 
				let rec f = function
					[]				-> raise (File_error "no files specified")
				  | file :: []		-> Printf.printf "latest attempt"; file#lookup id
				  | file :: files	-> 
						(try file#lookup id 
						with e -> Printf.printf "error: %s; recurring" (pretty_exn e); f files); 
						
				in
					f files

		  end
*)

