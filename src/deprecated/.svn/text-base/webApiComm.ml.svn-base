(*
 * Angband
 * Common daemon library
 * webApiComm.ml: client/server web API
 *
 * (C) 2005 Quendi srl.
 *)
 
open XThread
open ApiComm
open Printf
open Prelude

type session_key = string
type username = string
type passwd = string


(* abstract signatures *)

module type CustomApi =
  sig
  	type custom_cmd
  	type custom_ans
  	
  	val pretty_custom_cmd : custom_cmd -> string
  	val pretty_custom_ans : custom_ans -> string
  end

module type CustomFormat =
  sig
  	module CustomApi : CustomApi
  
  	val marshal_custom_cmd   : CustomApi.custom_cmd -> string
  	val marshal_custom_ans   : CustomApi.custom_ans -> string
  	val unmarshal_custom_cmd : string -> CustomApi.custom_cmd
  	val unmarshal_custom_ans : string -> CustomApi.custom_ans
  end   
  
module type CustomTextFormat =
  sig
  	module CustomApi : CustomApi
  
  	val stream_custom_cmd : CustomApi.custom_cmd -> string Stream.t
  	val stream_custom_ans : CustomApi.custom_ans -> string Stream.t
  	val parse_custom_cmd  : string Stream.t -> CustomApi.custom_cmd
  	val parse_custom_ans  : string Stream.t -> CustomApi.custom_ans
  end 
  
module type CustomInterpreter =
  sig
  	module CustomApi : CustomApi
  
  	type custom_global_env
  	type custom_session_env
  	
  	val is_authorized		    : Log.logger -> Unix.file_descr -> username -> passwd -> custom_global_env -> bool
  	val init_custom_session     : Log.logger -> Unix.file_descr -> username -> session_key -> custom_global_env -> custom_session_env
  	val finalize_custom_session : Log.logger -> Unix.file_descr -> username -> session_key -> custom_global_env -> custom_session_env -> unit
  	val interpret_custom_cmd    : Log.logger -> Unix.file_descr -> username -> session_key -> custom_global_env -> custom_session_env -> CustomApi.custom_cmd -> CustomApi.custom_ans
  end
  

(* functorial library *)

module MakeApi (CA : CustomApi) =
  struct
  	module CustomApi = CA
  
  	type cmd = Login of username * passwd
  	         | Logout of session_key
   	         | Custom_cmd of session_key * CA.custom_cmd
  	
  	type ans = Ack
  	         | Invalid_session_key
  	         | Illformed_cmd
  	         | Recoverable_exception of string
  			 | Logged_in of session_key option
  	         | Custom_ans of CA.custom_ans

  	let pretty_cmd = function
  		Login (uname, pwd) 	 -> sprintf "login %s %s" uname pwd
  	  | Logout _		   	 -> "logout"
  	  | Custom_cmd (_, ccmd) -> sprintf "customcmd %s" (CA.pretty_custom_cmd ccmd)
  	  
  	let pretty_ans = function
  		Ack					    -> "ack"
  	  | Logged_in None		    -> "loggedin none"
  	  | Logged_in (Some skey)   -> sprintf "loggedin %s" skey
  	  | Invalid_session_key	    -> "invalidsessionkey"
  	  | Illformed_cmd			-> "illformed_cmd"
  	  | Recoverable_exception s -> sprintf "recoverableexception %s" s
  	  | Custom_ans cans		    -> sprintf "customans %s" (CA.pretty_custom_ans cans)
  end  
  
 
module MakeTextFormat (CA : CustomApi)
					  (CTF : CustomTextFormat with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
  
   	let stream_cmd = function
  		Api.Login (uname, pwd) 		-> [< '"login"; 'uname; 'pwd >]
  	  | Api.Logout skey 	   	    -> [< '"logout"; 'skey >]
  	  | Api.Custom_cmd (skey, ccmd) -> [< '"customcmd"; 'skey; CTF.stream_custom_cmd ccmd >]
  	  
  	let stream_ans = function
  		Api.Ack					  	-> [< '"ack" >]
  	  | Api.Invalid_session_key	  	-> [< '"invalidsessionkey" >]
  	  | Api.Illformed_cmd			-> [< '"illformed" >]
  	  | Api.Recoverable_exception s -> [< '"recoverableexception"; 's >]
  	  |	Api.Logged_in skeyo    	  	-> [< '"loggedin"; stream_option stream_ident skeyo >]
  	  | Api.Custom_ans cans	  	  	-> [< '"customans"; CTF.stream_custom_ans cans >]
  	
  	let parse_cmd = parser
		[< '"login"; 'uname; 'pwd >] 	-> Api.Login (uname, pwd)
	  | [< '"logout"; 'skey >]			-> Api.Logout skey
  	  | [< '"customcmd"; 'skey; ccmd >]	-> Api.Custom_cmd (skey, CTF.parse_custom_cmd ccmd)
  	  
  	let parse_ans = parser
  		[< '"ack" >]			 	      -> Api.Ack
  	  | [< '"invalidsessionkey" >]		  -> Api.Invalid_session_key
  	  | [< '"illformed" >]				  -> Api.Illformed_cmd
  	  | [< '"recoverableexception"; 's >] -> Api.Recoverable_exception s
  	  | [< '"loggedin"; skeyo >] 	      -> Api.Logged_in (parse_option parse_ident skeyo)
  	  | [< '"customans"; cans >] 		  -> Api.Custom_ans (CTF.parse_custom_ans cans)
  end
 
  
module MakeFormat (CA : CustomApi)
				  (CTF : CustomFormat with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
  
  	let marshal_cmd cmd =
  		marsh (match cmd with
  				  Api.Custom_cmd (skey, ccmd) -> Latter (skey, CTF.marshal_custom_cmd ccmd)
  	  			| _				  	  		  -> Former cmd)
  	
  	let marshal_ans ans =
  		marsh (match ans with
  				  Api.Custom_ans cans -> Latter (CTF.marshal_custom_ans cans)
  	  			| _				  	  -> Former ans)
  	  
  	let unmarshal_cmd s =
  		match unmarsh s with
  			Former cmd       -> cmd
  	      | Latter (skey, s) -> Api.Custom_cmd (skey, CTF.unmarshal_custom_cmd s)
  	
  	let unmarshal_ans s =
  		match unmarsh s with
  			Former ans -> ans
  	      | Latter s   -> Api.Custom_ans (CTF.unmarshal_custom_ans s)
  end  

  
module MakePolyCustomFormat (CA : CustomApi) : CustomFormat with module CustomApi = CA =
  struct
  	module CustomApi = CA
  
  	let marshal_custom_cmd = marsh
  	let marshal_custom_ans = marsh
  	let unmarshal_custom_cmd = unmarsh
  	let unmarshal_custom_ans = unmarsh
  end
  
  
module MakeInterpreter (CA : CustomApi)
					   (CI : CustomInterpreter with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
  	
  	type session_env = unit
  	
  	type global_env = {
  		ge_custom_global_env : CI.custom_global_env;
  		ge_session_envs		 : (session_key, string * float * CI.custom_session_env) Hashtbl.t sync;
  	  }
  	
  	let fresh_session_key () = string_of_int (fresh ())
  	
	let interpret_login logger sock genv senv uname pwd =
		if CI.is_authorized logger sock uname pwd genv.ge_custom_global_env then begin
			let skey = fresh_session_key ()
			in
				try
					let csenv = CI.init_custom_session logger sock uname skey genv.ge_custom_global_env
					in
						genv.ge_session_envs#apply (fun self -> Hashtbl.add self skey (uname, Time.now (), csenv));
						logger#msg Log.Low (sprintf "user %S at peer %s logged in with session key %S" uname (Comm.pretty_peer sock) skey);
						Api.Logged_in (Some skey)
				with e ->
					logger#warn Log.High (sprintf "exception %S caught during custom session initialization of user %S. Refusing login from peer %s"
						(Printexc.to_string e) uname (Comm.pretty_peer sock));
					Api.Logged_in None
		  end
		else begin
			logger#msg Log.Normal (sprintf "user %S is not authorized. Login from peer %s refused" uname (Comm.pretty_peer sock));
			Api.Logged_in None
		  end
			
	let if_valid_session logger sock skey genv f =
		try
			let (uname, csenv) =
				genv.ge_session_envs#apply (fun self ->
					let (uname, _, csenv) = Hashtbl.find self skey in
					Hashtbl.replace self skey (uname, Time.now (), csenv);
					(uname, csenv))
			in
				f uname csenv
		with Not_found ->
			logger#warn Log.Normal (sprintf "invalid session key %S in command from peer %s. Connection shutdown" skey (Comm.pretty_peer sock));
		    Api.Invalid_session_key
	  
		  	  
	let interpret_logout logger sock genv senv skey =
		if_valid_session logger sock skey genv (fun uname csenv ->
			(try
				CI.finalize_custom_session logger sock skey uname genv.ge_custom_global_env csenv;
				logger#msg Log.Normal (sprintf "user %S at %s logged out with session key %S" uname (Comm.pretty_peer sock) skey)
			with e ->
				logger#warn Log.High (sprintf "exception %S caught during custom session finalization of user %S with session key %S. Forcing logout"
					(Printexc.to_string e) uname skey));
			genv.ge_session_envs#apply (fun self -> Hashtbl.remove self skey);
  			Api.Ack)
		  
  	let interpret_custom logger sock genv senv skey ccmd =
  		if_valid_session logger sock skey genv (fun uname csenv ->
  			try 
  				let cans = CI.interpret_custom_cmd logger sock uname skey genv.ge_custom_global_env csenv ccmd
  				in
  					Api.Custom_ans cans
  			with e ->
  				let s = Printexc.to_string e in
  				(logger#warn Log.High (sprintf "exception %S caught during custom interpreter of user %S with session key %S. Recovering..." s uname skey);
  				Api.Recoverable_exception s))
  					  
	let interpret_cmd logger sock genv senv = function
		Api.Login (uname, pwd) 		-> interpret_login logger sock genv senv uname pwd
	  | Api.Logout skey		   		-> interpret_logout logger sock genv senv skey
	  | Api.Custom_cmd (skey, ccmd) -> interpret_custom logger sock genv senv skey ccmd
	  
	let interpret_illformed_cmd logger sock genv senv cmd = Api.Illformed_cmd
	  
	let init_session logger sock genv = ()
  	let finalize_session logger sock genv senv = ()
  end
  

(* server side *)  

(*
 * ATTENZIONE!!! CODICE REPLICATO: apportare sempre modifiche doppie
 *)


module MakeCustomServer (CA : CustomApi)
						(CI : CustomInterpreter with module CustomApi = CA)
                        (A : Api)
						(F : Format with module Api = A)
                        (I : Interpreter with type global_env = CI.global_env with module Api = A)
                        (S : Server with module Interpreter = I) =
  struct
  	let make_global_env logger expiration cgenv =
  		let genv =
  			{ I.ge_custom_global_env = cgenv;
  			  I.ge_session_envs = new sync (Hashtbl.create 10) }
  		in
  		let h _ =
			genv.I.ge_session_envs#apply (fun tbl ->
				let expired = Hashtbl.fold (fun skey (uname, tm, _) skeys ->
					if Time.now () -. tm > expiration then begin
						logger#msg Log.High (sprintf "session key for user %S has expired" uname);
						skey :: skeys
					  end
					else skeys) tbl [];
				in
					List.iter (Hashtbl.remove tbl) expired)
   		in
  			ignore (Timer.add_handler logger ~interval:CommonConfig.web_session_key_collector_interval h Timer.Reload);
  			genv
	
  	let become_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
  		
  	let become_bg_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_bg_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
  end


module MakeBinServer (CA : CustomApi)
				 	 (CI : CustomInterpreter with module CustomApi = CA)
				 	 (CF : CustomFormat with module CustomApi = CA)
				 	 (T : Transport)
: sig
  	val become_server    : Log.logger -> ?queue:int -> ?name:string -> ?session_expiration:float -> CI.custom_global_env -> int -> unit
  	val become_bg_server : Log.logger -> ?queue:int -> ?name:string -> ?session_expiration:float -> CI.custom_global_env -> int -> Thread.t
  end
= struct
  	module A = MakeApi CA
	module F = MakeFormat CA CF
	module I = MakeInterpreter CA CI
	module S = MakeServer A I F T
  	
    module M = MakeCustomServer A F I S
    
	(*let make_global_env logger expiration cgenv =
  		let genv =
  			{ I.ge_custom_global_env = cgenv;
  			  I.ge_session_envs = new sync (Hashtbl.create 10) }
  		in
  		let h _ =
			genv.I.ge_session_envs#apply (fun tbl ->
				let expired = Hashtbl.fold (fun skey (uname, tm, _) skeys ->
					if Time.now () -. tm > expiration then begin
						logger#msg Log.High (sprintf "session key for user %S has expired" uname);
						skey :: skeys
					  end
					else skeys) tbl [];
				in
					List.iter (Hashtbl.remove tbl) expired)
   		in
  			ignore (Timer.add_handler logger ~interval:CommonConfig.web_session_key_collector_interval h Timer.Reload);
  			genv
	
  	let become_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
  		
  	let become_bg_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_bg_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
        *)
  end  
	
module MakeTextServer (CA : CustomApi)
			 	 	  (CI : CustomInterpreter with module CustomApi = CA)
				 	  (CTF : CustomTextFormat with module CustomApi = CA)
				 	  (S : sig val sep : char val quote : char end)
				 	  (T : Transport)
: sig
  	val become_server    : Log.logger -> ?queue:int -> ?name:string -> ?session_expiration:float -> CI.custom_global_env -> int -> unit
  	val become_bg_server : Log.logger -> ?queue:int -> ?name:string -> ?session_expiration:float -> CI.custom_global_env -> int -> Thread.t
  end
= struct
  	module A = MakeApi CA
	module I = MakeInterpreter CA CI
	module TF = MakeTextFormat CA CTF
  	module S = MakeServer A I (FormatOfTextFormat TF S) T
  
 	(*let make_global_env logger expiration cgenv =
  		let genv =
  			{ I.ge_custom_global_env = cgenv;
  			  I.ge_session_envs = new sync (Hashtbl.create 10) }
  		in
  		let h _ =
			genv.I.ge_session_envs#apply (fun tbl ->
				let expired = Hashtbl.fold (fun skey (uname, tm, _) skeys ->
					if Time.now () -. tm > expiration then begin
						logger#msg Log.High (sprintf "session key for user %S has expired" uname);
						skey :: skeys
					  end
					else skeys) tbl [];
				in
					List.iter (Hashtbl.remove tbl) expired)
   		in
  			ignore (Timer.add_handler logger ~interval:CommonConfig.web_session_key_collector_interval h Timer.Reload);
  			genv
	
  	let become_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
  		
  	let become_bg_server logger ?queue ?name ?(session_expiration = CommonConfig.default_web_session_expiration) cgenv port =
  		S.become_bg_server logger ?queue ?name (make_global_env logger session_expiration cgenv) port
        *)
  end    




  
 
  
(* client side *)
	
(*
 * ATTENZIONE!!! CODICE REPLICATO: apportare sempre modifiche doppie
 *)

module MakeBinClient (CA : CustomApi) 
					 (CF : CustomFormat with module CustomApi = CA)
				 	 (T : Transport)
: sig
	exception Invalid_session_key
	exception Illformed_cmd
	exception Recoverable_exception of string
	exception Pin_not_authorized
	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
	type conn
	val login      		: ?timeout:float -> string * string -> string -> int -> conn * session_key
	val logout	   	    : ?timeout:float -> conn -> session_key -> unit
	val disconnect 		: conn -> unit
	val exec_custom_cmd	: ?timeout:float -> conn -> session_key -> CA.custom_cmd -> CA.custom_ans
  end
= struct
  	module A = MakeApi CA
	module F = MakeFormat CA CF
  	module C = MakeClient A F T
  
	exception Invalid_session_key
	exception Illformed_cmd
	exception Recoverable_exception of string
	exception Pin_not_authorized
	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
		
	type conn = C.conn
	
	let disconnect = C.disconnect
	
	let login ?timeout (uname, pwd) addr port =
		let conn = try C.connect addr port
				   with (C.Cannot_resolve s)     -> raise (Cannot_resolve s)
  		             |  (C.Connection_refused s) -> raise (Connection_refused s)
		in
			match C.exec_cmd ?timeout conn (A.Login (uname, pwd)) with
				A.Logged_in (Some skey)	-> (conn, skey)
			  | A.Logged_in None		-> raise Pin_not_authorized
			  | ans						-> unexpected_error "MakeWebClient.login" (sprintf "bad answer %S to login command" (A.pretty_ans ans))

	let exec_cmd ?timeout conn cmd =
		try C.exec_cmd ?timeout conn cmd
		with C.Connection_timed_out -> raise Connection_timed_out
		  |  C.Connection_broken    -> raise Connection_broken		  
			  
	let logout ?timeout conn skey =
		match exec_cmd ?timeout conn (A.Logout skey) with
			A.Ack -> ()
		  | ans   -> unexpected_error "MakeWebClient.logout" (sprintf "bad answer %S to logout command" (A.pretty_ans ans))

	let exec_custom_cmd ?timeout conn skey ccmd =
		match exec_cmd ?timeout conn (A.Custom_cmd (skey, ccmd)) with
			A.Custom_ans cans		  -> cans
		  | A.Invalid_session_key	  -> raise Invalid_session_key
		  | A.Illformed_cmd			  -> raise Illformed_cmd
		  | A.Recoverable_exception s -> raise (Recoverable_exception s)
		  | ans 				      -> unexpected_error "MakeWebClient.exec_custom_cmd" (sprintf "bad answer %S to custom command %S" (A.pretty_ans ans) (CA.pretty_custom_cmd ccmd))
  end
  
module MakeTextClient (CA : CustomApi) 
		 			  (CTF : CustomTextFormat with module CustomApi = CA)
		 			  (S : sig val sep : char val quote : char end)
				 	  (T : Transport)
: sig
	exception Invalid_session_key
	exception Illformed_cmd
	exception Recoverable_exception of string
	exception Pin_not_authorized
	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
	type conn
	val login      		: ?timeout:float -> string * string -> string -> int -> conn * session_key
	val logout	   	    : ?timeout:float -> conn -> session_key -> unit
	val disconnect 		: conn -> unit
	val exec_custom_cmd	: ?timeout:float -> conn -> session_key -> CA.custom_cmd -> CA.custom_ans
  end
= struct
  	module A = MakeApi CA
	module TF = MakeTextFormat CA CTF
  	module C = MakeClient A (FormatOfTextFormat TF S) T
  
	exception Invalid_session_key
	exception Illformed_cmd
	exception Recoverable_exception of string
	exception Pin_not_authorized
	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
		
	type conn = C.conn
	
	let disconnect = C.disconnect
	
	let login ?timeout (uname, pwd) addr port =
		let conn = try C.connect addr port
				   with C.Cannot_resolve s     -> raise (Cannot_resolve s)
  		             |  C.Connection_refused s -> raise (Connection_refused s)
		in
			match C.exec_cmd ?timeout conn (A.Login (uname, pwd)) with
				A.Logged_in (Some skey)	-> (conn, skey)
			  | A.Logged_in None		-> raise Pin_not_authorized
			  | ans						-> unexpected_error "MakeWebClient.login" (sprintf "bad answer %S to login command" (A.pretty_ans ans))

	let exec_cmd ?timeout conn cmd =
		try C.exec_cmd ?timeout conn cmd
		with C.Connection_timed_out -> raise Connection_timed_out
		  |  C.Connection_broken    -> raise Connection_broken		  
			  
	let logout ?timeout conn skey =
		match exec_cmd ?timeout conn (A.Logout skey) with
			A.Ack -> ()
		  | ans   -> unexpected_error "MakeWebClient.logout" (sprintf "bad answer %S to logout command" (A.pretty_ans ans))

	let exec_custom_cmd ?timeout conn skey ccmd =
		match exec_cmd ?timeout conn (A.Custom_cmd (skey, ccmd)) with
			A.Custom_ans cans		  -> cans
		  | A.Invalid_session_key	  -> raise Invalid_session_key
		  | A.Illformed_cmd			  -> raise Illformed_cmd
		  | A.Recoverable_exception s -> raise (Recoverable_exception s)
		  | ans 				      -> unexpected_error "MakeWebClient.exec_custom_cmd" (sprintf "bad answer %S to custom command %S" (A.pretty_ans ans) (CA.pretty_custom_cmd ccmd))
  end

	
	
(* typical clients and servers *)	
	
module MakeBinPeers (CA : CustomApi)
		 	   	    (CF : CustomFormat with module CustomApi = CA)
				   	(T : Transport) =
  struct
	module Server (CI : CustomInterpreter with module CustomApi = CA) = MakeBinServer CA CI CF T
	module Client = MakeBinClient CA CF T
  end
	
module MakeTextPeers (CA : CustomApi)
			 	     (CTF : CustomTextFormat with module CustomApi = CA)
			 	     (S : sig val sep : char val quote : char end)
				     (T : Transport) =
  struct
	module Server (CI : CustomInterpreter with module CustomApi = CA) = MakeTextServer CA CI CTF S T
	module Client = MakeTextClient CA CTF S T
  end  
  
module MakeRawPeers (CA : CustomApi) (E : Encoder) =
	MakeBinPeers CA (MakePolyCustomFormat CA) (EncodeTransport E RawTransport)
  
module MakeLinePeers (CA : CustomApi)
					 (CTF : CustomTextFormat with module CustomApi = CA)
					 (T : sig val sep : char val quote : char val term : char end) =
	MakeTextPeers CA CTF T (LineTransport T)
