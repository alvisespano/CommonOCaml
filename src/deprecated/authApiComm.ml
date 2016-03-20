(*
 * Angband
 * Common daemon library
 * authApiComm.ml: client/server authentication API
 *
 * (C) 2005 Quendi srl.
 *)
 
open XThread
open ApiComm
open Printf
open Prelude
open XList

 
(* abstract signatures *)

type pin = string

module type CustomApi =
  sig
  	type custom_cmd
  	type custom_ans
  	type failure
  	
  	val pretty_custom_cmd : custom_cmd -> string
  	val pretty_custom_ans : custom_ans -> string
  	val pretty_failure	  : failure -> string
  end

module type CustomFormat =
  sig
  	module CustomApi : CustomApi
  
  	val marshal_custom_cmd   : CustomApi.custom_cmd -> string
  	val marshal_custom_ans   : CustomApi.custom_ans -> string
  	val marshal_failure   	 : CustomApi.failure -> string
  	val unmarshal_custom_cmd : string -> CustomApi.custom_cmd
  	val unmarshal_custom_ans : string -> CustomApi.custom_ans
  	val unmarshal_failure 	 : string -> CustomApi.failure
  end   
  
module type CustomTextFormat =
  sig
  	module CustomApi : CustomApi
  
  	val stream_custom_cmd : CustomApi.custom_cmd -> string Stream.t
  	val stream_custom_ans : CustomApi.custom_ans -> string Stream.t
  	val stream_failure	  : CustomApi.failure -> string Stream.t
  	val parse_custom_cmd  : string Stream.t -> CustomApi.custom_cmd
  	val parse_custom_ans  : string Stream.t -> CustomApi.custom_ans
  	val parse_failure	  : string Stream.t -> CustomApi.failure
  end 
  
module type CustomAuth =
  sig
  	val challenge_response : pin -> string -> string
  end
  
module type CustomInterpreter =
  sig
  	module CustomApi : CustomApi
  
  	type custom_global_env
  	type custom_session_env
  	
  	val recover					: exn -> CustomApi.failure option
  	val is_authorized		    : Log.logger -> Unix.file_descr -> pin -> custom_global_env -> bool
  	val challenge				: Log.logger -> Unix.file_descr -> pin -> custom_global_env -> string
  	val init_custom_session     : Log.logger -> Unix.file_descr -> pin -> custom_global_env -> custom_session_env
  	val finalize_custom_session : Log.logger -> Unix.file_descr -> pin -> custom_global_env -> custom_session_env -> unit
  	val interpret_custom_cmd    : Log.logger -> Unix.file_descr -> pin -> custom_global_env -> custom_session_env -> CustomApi.custom_cmd -> CustomApi.custom_ans
  end

type session_mode = Exclusive_session_without_resume | Non_exclusive_session

type login_mode = Simple_login | Secure_login
    
module type AuthServer =
  sig
  	type custom_global_env
  
  	val become_server    	 : Log.logger -> queue:int -> ?name:string -> session_mode -> login_mode -> custom_global_env -> int -> unit
  	val become_bg_server 	 : Log.logger -> queue:int -> ?name:string -> session_mode -> login_mode -> custom_global_env -> int -> Thread.t
    val connect_and_serve 	 : Log.logger -> session_mode -> login_mode -> custom_global_env -> string -> int -> unit
    val connect_and_bg_serve : Log.logger -> name:string -> session_mode -> login_mode -> custom_global_env -> string -> int -> Thread.t
  end

module type AuthClient =
  sig
  	module CustomApi : CustomApi	

	exception Unauthorized_cmd of CustomApi.custom_cmd
	exception Illformed_cmd of CustomApi.custom_cmd
	exception Recoverable_failure of CustomApi.failure
	exception Unrecovered_exception of string
	exception Pin_not_authorized of string
	exception Authentication_failed
	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
 
 	type conn 	
 
	val connect         : string -> int -> conn
	val disconnect 		: conn -> unit
    val accept_server   : Log.logger -> int -> conn
	val login      		: Log.logger -> ?timeout:float -> conn -> pin -> unit
	val logout	   	    : Log.logger -> ?timeout:float -> conn -> unit
	val exec_custom_cmd	: Log.logger -> ?timeout:float -> conn -> CustomApi.custom_cmd -> CustomApi.custom_ans
  end
  
  

(* functorial library *)

module MakeApi (CA : CustomApi) =
  struct  
  	module CustomApi = CA
  
   	type cmd = Login of pin
   	         | Challenge_response of string
  			 | Logout
  			 | Custom_cmd of CA.custom_cmd
  	
  	type ans = Ack
  	 		 | Challenge of string
  	         | Unauthorized_cmd
  	         | Illformed_cmd
  	         | Recoverable_failure of CA.failure
  	         | Unrecovered_exception of string
  			 | Logged_in of bool
  	         | Custom_ans of CA.custom_ans

  	let pretty_cmd = function
  		Login pin				-> sprintf "login %s" pin
  	  | Challenge_response s	-> sprintf "challengeresponse %S" s
  	  | Logout					-> "logout"
  	  | Custom_cmd ccmd			-> sprintf "customcmd %s" (CA.pretty_custom_cmd ccmd)
  	  
  	let pretty_ans = function
  		Ack					  	-> "ack"
  	  | Challenge s				-> sprintf "challenge %S" s
  	  | Logged_in b			  	-> sprintf "loggedin %b" b
  	  | Unauthorized_cmd		-> "unauthorizedcmd"
  	  | Illformed_cmd			-> "illformedcmd"
  	  | Recoverable_failure f   -> sprintf "recoverablefailure %S" (CA.pretty_failure f)
  	  | Unrecovered_exception s -> sprintf "unrecoveredexception %S" s
  	  | Custom_ans cans		  	-> sprintf "customans %s" (CA.pretty_custom_ans cans)
  end  
  
  
module MakeTextFormat (CA : CustomApi)
					  (CTF : CustomTextFormat with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
  
  	let stream_cmd = function
  		Api.Login pin				-> [< '"login"; 'pin >]
  	  | Api.Challenge_response s	-> [< '"challengeresponse"; 's >]
  	  | Api.Logout 	  	    		-> [< '"logout" >]
  	  | Api.Custom_cmd ccmd 		-> [< '"customcmd"; CTF.stream_custom_cmd ccmd >]
  	
  	let stream_ans = function
  		Api.Ack					  	-> [< '"ack" >]
  	  | Api.Challenge s				-> [< '"challenge"; 's >]
  	  | Api.Unauthorized_cmd		-> [< '"unauthorizedcmd" >]
  	  | Api.Illformed_cmd			-> [< '"illformedcmd" >]
  	  | Api.Recoverable_failure f 	-> [< '"recoverablefailure"; CTF.stream_failure f >]
  	  | Api.Unrecovered_exception s -> [< '"unrecoveredexception"; 's >]
  	  |	Api.Logged_in b	    	  	-> [< '"loggedin"; 'string_of_bool b >]
  	  | Api.Custom_ans cans	  	  	-> [< '"customans"; CTF.stream_custom_ans cans >]
  	
  	let parse_cmd = parser
		[< '"login"; 'pin >] 	 		-> Api.Login pin
	  |	[< '"challengeresponse"; 's >]	-> Api.Challenge_response s
	  | [< '"logout" >]			 		-> Api.Logout
  	  | [< '"customcmd"; ccmd >] 		-> Api.Custom_cmd (CTF.parse_custom_cmd ccmd)
	  	  		 
  	let parse_ans = parser
  		[< '"ack" >]			 	      -> Api.Ack
  	  |	[< '"challenge"; 's >]			  -> Api.Challenge s
  	  | [< '"unauthorizedcmd" >]		  -> Api.Unauthorized_cmd
  	  | [< '"illformedcmd" >]			  -> Api.Illformed_cmd
  	  | [< '"recoverablefailure"; f >]	  -> Api.Recoverable_failure (CTF.parse_failure f)
  	  | [< '"unrecoveredexception"; 's >] -> Api.Unrecovered_exception s
  	  | [< '"loggedin"; 'b >]	 	      -> Api.Logged_in (bool_of_string b)
  	  | [< '"customans"; cans >] 	      -> Api.Custom_ans (CTF.parse_custom_ans cans)
  end
 
  
module MakeFormat (CA : CustomApi) (CF : CustomFormat with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
   	
  	let marshal_cmd cmd =
  		marsh (match cmd with
  				  Api.Custom_cmd ccmd 	   -> `Custom_cmd (CF.marshal_custom_cmd ccmd)
  	  			| Api.Login _
  	  			| Api.Logout	
  	  			| Api.Challenge_response _ -> `Simple cmd
  	  			)
  	
  	let marshal_ans ans =
  		marsh (match ans with
  				  Api.Custom_ans cans 		  -> `Custom_ans (CF.marshal_custom_ans cans)
  	  			| Api.Recoverable_failure f	  -> `Recoverable_failure (CF.marshal_failure f)
  	  			| Api.Unrecovered_exception _
  	  			| Api.Ack
  	  			| Api.Challenge _
  	  			| Api.Unauthorized_cmd
  	  			| Api.Illformed_cmd
  	  			| Api.Logged_in _			  -> `Simple ans
  	  			)
  	  
  	let unmarshal_cmd s =
  		match unmarsh s with
  			`Simple cmd   -> cmd
  	      | `Custom_cmd s -> Api.Custom_cmd (CF.unmarshal_custom_cmd s)
  	
  	let unmarshal_ans s =
  		match unmarsh s with
  			`Simple ans   		   -> ans
  	      | `Custom_ans s 		   -> Api.Custom_ans (CF.unmarshal_custom_ans s)
  	      | `Recoverable_failure s -> Api.Recoverable_failure (CF.unmarshal_failure s)
  end  
  

module MakePolyCustomFormat (CA : CustomApi) =
  struct
  	module CustomApi = CA
  
  	let marshal_custom_cmd = marsh
  	let marshal_custom_ans = marsh
  	let marshal_failure = marsh
  	let unmarshal_custom_cmd = unmarsh
  	let unmarshal_custom_ans = unmarsh
  	let unmarshal_failure = unmarsh
  end  
  

module MakeInterpreter (CA : CustomApi)
					   (CAu : CustomAuth)
					   (CI : CustomInterpreter with module CustomApi = CA) =
  struct
  	module Api = MakeApi CA
  		
  	let is_exclusive = function
  		Exclusive_session_without_resume -> true
  	  | Non_exclusive_session			 -> false
  	
  	type global_env = {
  		ge_custom_global_env : CI.custom_global_env;
  		ge_logged_in		 : pin list sync;
  		ge_session_mode		 : session_mode;
        ge_login_mode		 : login_mode;
  	  }
  	
  	type phase = Not_logged_in
  			   | Authenticating of pin * string
  			   | Logged_in of pin * CI.custom_session_env
  	  
  	type session_env = {
  		mutable se_phase : phase;
  	  }

  	let interpret_login logger sock genv senv pin =
		match senv.se_phase with
			Not_logged_in as p ->
				if CI.is_authorized logger sock pin genv.ge_custom_global_env then begin
					let f self =
						if is_exclusive genv.ge_session_mode && occurs pin self then begin
							genv.ge_logged_in#set (pin :: self);
							`Already
						  end
						else
                        	match genv.ge_login_mode with
                            	Simple_login -> `Ok
                              | Secure_login -> `Challenge
					in
						match genv.ge_logged_in#apply f with
							`Already -> 
								logger#msg Log.Normal (sprintf "client pin %S is already logged in. Login from peer %s refused" pin (Comm.pretty_peer sock));
						  		(Api.Logged_in false, p)
						  		
						  | `Challenge ->
								let ch = CI.challenge logger sock pin genv.ge_custom_global_env
								in
									(Api.Challenge ch, Authenticating (pin, ch))
                                    
                          | `Ok ->
                          		try
		  							let csenv = CI.init_custom_session logger sock pin genv.ge_custom_global_env
		  							in
		  								logger#msg Log.Normal (sprintf "client pin %S successfully logged in" pin);
		  								(Api.Logged_in true, Logged_in (pin, csenv))
		  						with e ->
		  							logger#warn Log.High (sprintf "exception caught during custom session initialization of client pin %S: %s. Connection shutdown"
  										(Printexc.to_string e) pin);
  									raise Exit
                  end
				else begin
					logger#msg Log.Normal (sprintf "client pin %S is not authorized. Login from peer %s refused" pin (Comm.pretty_peer sock));
					(Api.Logged_in false, Not_logged_in)
				  end
					  
		 | Authenticating (pin, ch) as p ->
				logger#msg Log.Normal (sprintf "client pin %S retried login but still needs authentication. Replying with challenge again..." pin);
				(Api.Challenge ch, p)
				
		 | Logged_in (pin, csenv) as p ->
		 		logger#msg Log.Normal (sprintf "client pin %S tried login but was already authenticated" pin);
				(Api.Ack, p)
		 		 
			  
	let interpret_challenge_response logger sock genv senv chr =
		match senv.se_phase with
			Not_logged_in as p ->
				logger#msg Log.Normal (sprintf "client peer %s attempted challenge response but was not logged in" (Comm.pretty_peer sock));
				(Api.Unauthorized_cmd, p)
				
		  | Authenticating (pin, ch) ->
		  		if CAu.challenge_response pin ch = chr then begin
		  			try
		  				let csenv = CI.init_custom_session logger sock pin genv.ge_custom_global_env
		  				in
		  					logger#msg Log.Normal (sprintf "client pin %S successfully authenticated" pin);
		  					(Api.Logged_in true, Logged_in (pin, csenv))
		  			with e ->
		  				logger#warn Log.High (sprintf "exception caught during custom session initialization of client pin %S: %s. Connection shutdown"
  								(Printexc.to_string e) pin);
  						raise Exit
		  		  end
		  		else begin
		  			logger#msg Log.Normal (sprintf "client pin %S failed authentication. Connection shutdown" pin);
		  		 	raise Exit
		  		  end
		  		  
		  | Logged_in (pin, csenv) as p ->
				logger#msg Log.Normal (sprintf "client pin %S tried challenge response but was already authenticated" pin);
				(Api.Logged_in true, p)
	 		  
				
	let interpret_logout logger sock genv senv =
		match senv.se_phase with
			Not_logged_in as p ->
				logger#msg Log.Normal (sprintf "client peer %S tried logout but is not logged in" (Comm.pretty_peer sock));
			    (Api.Unauthorized_cmd, p) 
			
		  | Authenticating (pin, ch) ->
		  		logger#msg Log.Normal (sprintf "client pin %S prematurely logged out before authentication" pin);
			    (Api.Ack, Not_logged_in) 
		  		  
		  | Logged_in (pin, csenv) ->
		  		(try
					CI.finalize_custom_session logger sock pin genv.ge_custom_global_env csenv;
					logger#msg Log.Normal (sprintf "client pin %S logged out" pin)
				with e ->
					logger#warn Log.High (sprintf "exception caught during custom session finalization of client pin %S: %s. Forcing logout"
						(Printexc.to_string e) pin));
				genv.ge_logged_in#apply_and_set (remove pin);
				(Api.Ack, Not_logged_in)
				
			
  	let interpret_custom logger sock genv senv ccmd =
  		match senv.se_phase with
			Not_logged_in as p ->
				logger#msg Log.Normal (sprintf "client peer %s tried custom command but is not logged in" (Comm.pretty_peer sock));
				(Api.Unauthorized_cmd, p)		
			
		  | Authenticating (pin, ch) as p ->
		  		logger#msg Log.Normal (sprintf "client pin %S tried custom command but still needs authentication" pin);
				(Api.Unauthorized_cmd, p)
		  				  
		  | Logged_in (pin, csenv) as p ->		
			  	let ans =
				  	try Api.Custom_ans (CI.interpret_custom_cmd logger sock pin genv.ge_custom_global_env csenv ccmd)
		  			with e ->
		  				logger#warn Log.High (sprintf "exception caught during custom interpreter of client pin %S: %s. Recovering..." pin (Printexc.to_string e));
		  				match CI.recover e with
		  					Some f -> Api.Recoverable_failure f
		  				  | None   -> Api.Unrecovered_exception (Printexc.to_string e)
  				in
  					(ans, p)
  					
  					
	let interpret_cmd logger sock genv senv cmd =
		let (ans, phase) =
			match cmd with
				Api.Login pin			 -> interpret_login logger sock genv senv pin
			  | Api.Challenge_response s -> interpret_challenge_response logger sock genv senv s
			  | Api.Logout 				 -> interpret_logout logger sock genv senv
			  | Api.Custom_cmd ccmd		 -> interpret_custom logger sock genv senv ccmd
		in
			senv.se_phase <- phase;
			ans
			  	  
	let interpret_illformed_cmd logger sock genv senv cmd = Api.Illformed_cmd
	  
	let init_session logger sock genv = { se_phase = Not_logged_in }
  	
  	let finalize_session logger sock genv senv =
  		match senv.se_phase with
  			Logged_in (pin, csenv) ->
  				CI.finalize_custom_session logger sock pin genv.ge_custom_global_env csenv;
  				genv.ge_logged_in#apply_and_set (remove pin)
  				
  		  | Authenticating _
  		  | Not_logged_in	 -> ()
  end
  

(* server side *)  
  
module MakeAuthServer (CA : CustomApi)
					  (CAu : CustomAuth)
				 	  (CI : CustomInterpreter with module CustomApi = CA) =
  struct
  	module A = MakeApi CA
	module I = MakeInterpreter CA CAu CI
    
	module Make (F : Format with type Api.cmd = A.cmd with type Api.ans = A.ans) (T : Transport) =
      struct
      	module S = MakeServer A I F T

		type custom_global_env = CI.custom_global_env
  	
	    let make_global_env session_mode login_mode cgenv =
  		    { I.ge_custom_global_env = cgenv;
  		      I.ge_logged_in = new sync [];
  		      I.ge_session_mode = session_mode;
              I.ge_login_mode = login_mode }

  	    let become_server logger ~queue ?name session_mode login_mode cgenv port =
        	S.become_server logger ~queue ?name (make_global_env session_mode login_mode cgenv) port
            
  	    let become_bg_server logger ~queue ?name session_mode login_mode cgenv port =
        	S.become_bg_server logger ~queue ?name (make_global_env session_mode login_mode cgenv) port
            
        let connect_and_serve logger session_mode login_mode cgenv addr port =
        	S.connect_and_serve logger (make_global_env session_mode login_mode cgenv) addr port
            
        let connect_and_bg_serve logger ~name session_mode login_mode cgenv addr port =
        	S.connect_and_bg_serve logger ~name (make_global_env session_mode login_mode cgenv) addr port
    
	  end
  end
  
module MakeBinAuthServer (CA : CustomApi)
					     (CAu : CustomAuth)
				 	     (CI : CustomInterpreter with module CustomApi = CA)
				 	     (CF : CustomFormat with module CustomApi = CA)
				 	     (T : Transport)
                         : AuthServer with type custom_global_env = CI.custom_global_env
= struct
  	module AS = MakeAuthServer CA CAu CI
    module Main = AS.Make (MakeFormat CA CF) T
    include Main
  end
  
module MakeTextAuthServer (CA : CustomApi)
					  	  (CAu : CustomAuth)
			 	 	  	  (CI : CustomInterpreter with module CustomApi = CA)
				 	  	  (CTF : CustomTextFormat with module CustomApi = CA)
				 	  	  (S : sig val sep : char val quote : char end)
				 	  	  (T : Transport)
                          : AuthServer with type custom_global_env = CI.custom_global_env                     
= struct
  	module AS = MakeAuthServer CA CAu CI
    module Main = AS.Make (FormatOfTextFormat (MakeTextFormat CA CTF) S) T
    include Main
  end

  
  
(* client side *) 
 
module MakeAuthClient (CA : CustomApi) (CAu : CustomAuth) =
  struct
	module A = MakeApi CA
    
    module Make (F : Format with type Api.cmd = A.cmd with type Api.ans = A.ans) (T : Transport) =
      struct
		module C = MakeClient A F T
  		module CustomApi = CA
	    
	    exception Unauthorized_cmd of CA.custom_cmd
	    exception Illformed_cmd of CA.custom_cmd
	    exception Recoverable_failure of CA.failure
	    exception Unrecovered_exception of string
	    exception Pin_not_authorized of string
	    exception Authentication_failed
	    exception Cannot_resolve of string
  	    exception Connection_refused of string
  	    exception Connection_broken
  	    exception Connection_timed_out

		type conn = C.conn

		let connect = C.connect
	    let disconnect = C.disconnect
		let accept_server = C.accept_server

	    let login logger ?timeout conn pin =
			match C.exec_cmd logger ?timeout conn (A.Login pin) with
				A.Challenge ch -> 
					let chr = CAu.challenge_response pin ch
					in
						(match C.exec_cmd logger ?timeout conn (A.Challenge_response chr) with
							A.Logged_in true  -> ()
						  | A.Logged_in false -> raise Authentication_failed
						  | ans				  -> raise (Unexpected (sprintf "bad answer to challenge_response command: %s" (A.pretty_ans ans)))
                          )

			  | A.Logged_in false -> raise (Pin_not_authorized pin)

			  | A.Logged_in true  -> ()

			  | ans				  -> raise (Unexpected (sprintf "bad answer to login command: %s" (A.pretty_ans ans)))


	    let exec_cmd logger ?timeout conn cmd =
		    try C.exec_cmd logger ?timeout conn cmd
		    with C.Connection_timed_out -> raise Connection_timed_out
		      |  C.Connection_broken    -> raise Connection_broken

	    let logout logger ?timeout conn =
		    match exec_cmd logger ?timeout conn A.Logout with
			    A.Ack -> ()
		      | ans   -> raise (Unexpected (sprintf "bad answer to logout command: %s" (A.pretty_ans ans)))

	    let exec_custom_cmd logger ?timeout conn ccmd =
		    match exec_cmd logger ?timeout conn (A.Custom_cmd ccmd) with
			    A.Custom_ans cans		  -> cans
		      | A.Unauthorized_cmd		  -> raise (Unauthorized_cmd ccmd)
		      | A.Illformed_cmd			  -> raise (Illformed_cmd ccmd)
		      | A.Recoverable_failure f   -> raise (Recoverable_failure f)
		      | A.Unrecovered_exception s -> raise (Unrecovered_exception s)
		      | ans 				      -> raise (Unexpected (sprintf "bad answer to custom command %s: %s" (A.pretty_ans ans) (CA.pretty_custom_cmd ccmd)))
      end
  end
  
module MakeBinAuthClient (CA : CustomApi)
					     (CAu : CustomAuth)
				 	     (CF : CustomFormat with module CustomApi = CA)
				 	     (T : Transport)
                         : AuthClient with module CustomApi = CA
= struct
  	module AC = MakeAuthClient CA CAu
    module Main = AC.Make (MakeFormat CA CF) T
    include Main
  end
  
module MakeTextAuthClient (CA : CustomApi)
					  	  (CAu : CustomAuth)
			 	 	  	  (CTF : CustomTextFormat with module CustomApi = CA)
				 	  	  (S : sig val sep : char val quote : char end)
				 	  	  (T : Transport)
                          : AuthClient with module CustomApi = CA                     
= struct
  	module AS = MakeAuthClient CA CAu
    module Main = AS.Make (FormatOfTextFormat (MakeTextFormat CA CTF) S) T
    include Main
  end

  
	
(* typical clients and servers *)	
	
module MakeBinAuthClientServer (CA : CustomApi)
				    	   	   (CAu : CustomAuth)
		 	   	    	   	   (CF : CustomFormat with module CustomApi = CA)
				   		   	   (T : Transport) =
  struct
	module Server (CI : CustomInterpreter with module CustomApi = CA) = MakeBinAuthServer CA CAu CI CF T
	module Client = MakeBinAuthClient CA CAu CF T
  end
	
module MakeTextAuthClientServer (CA : CustomApi)
					 		    (CAu : CustomAuth)
			 	                (CTF : CustomTextFormat with module CustomApi = CA)
			 	                (S : sig val sep : char val quote : char end)
				                (T : Transport) =
  struct
	module Server (CI : CustomInterpreter with module CustomApi = CA) = MakeTextAuthServer CA CAu CI CTF S T
	module Client = MakeTextAuthClient CA CAu CTF S T
  end  
  
module MakeRawClientServer (CA : CustomApi) (CAu : CustomAuth) (E : Encoder) =
	MakeBinAuthClientServer CA CAu (MakePolyCustomFormat CA) (EncodeTransport E RawTransport)
  
module MakeLineAuthClientServer (CA : CustomApi)
					            (CAu : CustomAuth)
					            (CTF : CustomTextFormat with module CustomApi = CA)
					            (T : sig val sep : char val quote : char val term : char end) =
	MakeTextAuthClientServer CA CAu CTF T (LineTransport T)
	
module MakeCipherCustomAuth (S : sig val secret : Security.secret end) =
  struct
  	open Security
  	let challenge_response pin ch =	marsh (encrypt (secret_of_string pin) (encrypt S.secret ch))
  end
 
