(*
 * Angband
 * Common daemon library
 * apiComm.ml: client/server API
 *
 * (C) 2005 Quendi srl.
 *)

open Prelude
open Printf
open XString
open XStream

(* abstract signatures *)

module type Api =
  sig
  	type cmd
  	type ans
  	
  	val pretty_cmd : cmd -> string
  	val pretty_ans : ans -> string
  end
  
module type Encoder =
  sig
  	val encode : string -> string
  	val decode : string -> string
  end  
  
module type Transport =
  sig
  	val send    : Unix.file_descr -> string -> unit
  	val receive : ?timeout:float -> Unix.file_descr -> string
  end  
  
module type Format =
  sig
  	module Api : Api
  	
  	val marshal_cmd   : Api.cmd -> string
  	val marshal_ans	  : Api.ans -> string
  	val unmarshal_cmd : string -> Api.cmd
  	val unmarshal_ans : string -> Api.ans
  end  
  
module type TextFormat =
  sig
  	module Api : Api
  	
  	val stream_cmd : Api.cmd -> string Stream.t
  	val stream_ans : Api.ans -> string Stream.t
  	val parse_cmd  : string Stream.t -> Api.cmd
  	val parse_ans  : string Stream.t -> Api.ans
  end   
 
module type Interpreter =
  sig
  	module Api : Api
  
  	type global_env
  	type session_env
  	
  	val init_session	 		: Log.logger -> Unix.file_descr -> global_env -> session_env
  	val finalize_session 		: Log.logger -> Unix.file_descr -> global_env -> session_env -> unit
  	val interpret_cmd	 		: Log.logger -> Unix.file_descr -> global_env -> session_env -> Api.cmd -> Api.ans
  	val interpret_illformed_cmd : Log.logger -> Unix.file_descr -> global_env -> session_env -> string -> Api.ans
  end
  
  
(* typical transports *)

module LineTransport (T : sig val term : char end) : Transport =
  struct
  	let send sock line = Comm.send_line T.term sock (escape [T.term] line)
  	let receive ?timeout sock = unescape [T.term] (Comm.receive_line ?timeout T.term sock)
  end
  
module RawTransport : Transport =
  struct
  	let send = Comm.send_raw
    let receive ?timeout = Comm.receive_raw ?timeout
  end
  
 
  
(* functorial library *)  
  
module FormatOfTextFormat (TF : TextFormat) (S : sig val sep : char val quote : char end) : Format with module Api = TF.Api =
  struct
  	module Api = TF.Api
  
  	let marshal_cmd cmd = concat_stream ~sep:S.sep ~quote:S.quote (TF.stream_cmd cmd)
	let marshal_ans ans = concat_stream ~sep:S.sep ~quote:S.quote (TF.stream_ans ans)
	
	let unmarshal_cmd s = TF.parse_cmd (tokenize ~sep:S.sep ~quote:S.quote s)
	let unmarshal_ans s = TF.parse_ans (tokenize ~sep:S.sep ~quote:S.quote s)
  end

module MakePolyFormat (A : Api) : Format with module Api = A =
  struct
  	module Api = A
  
  	let marshal_cmd = marsh
  	let marshal_ans = marsh
  	let unmarshal_cmd = unmarsh
  	let unmarshal_ans = unmarsh
  end
  
module Encrypter (S : sig val secret : Security.secret end) : Encoder =
  struct
  	open Security
  	let encode s = marsh (encrypt S.secret s)
  	let decode s = decrypt S.secret (unmarsh s)
  end
 
module Cruncher : Encoder =
  struct
  	open Security
  	let encode s = marsh (crunch s)
  	let decode s = decrunch (unmarsh s)
  end
  
module DummyEncoder : Encoder = struct let encode = ident let decode = ident end
  
module PipeEncoder (E1 : Encoder) (E2 : Encoder) : Encoder =
  struct
  	let encode s = E2.encode (E1.encode s)
  	let decode s = E1.decode (E2.decode s)
  end
   
module EncodeFormat (E : Encoder) (F : Format) : Format (*with module Api = F.Api*) =
  struct
  	module Api = F.Api
  	
  	let marshal_cmd cmd = E.encode (F.marshal_cmd cmd)
  	let marshal_ans ans = E.encode (F.marshal_ans ans)
  	let unmarshal_cmd s = F.unmarshal_cmd (E.decode s)
  	let unmarshal_ans s = F.unmarshal_ans (E.decode s)
  end
    
module EncryptFormat (S : sig val secret : Security.secret end) = EncodeFormat (Encrypter S)

module CrunchFormat = EncodeFormat Cruncher

module CrunchAndEncryptFormat (S : sig val secret : Security.secret end) (F : Format) =
	EncryptFormat S (CrunchFormat F)
  
module EncodeTransport (E : Encoder) (T : Transport) : Transport =
  struct
  	let send sock data = T.send sock (E.encode data)
  	let receive ?timeout sock = E.decode (T.receive ?timeout sock)
  end

module EncryptTransport (S : sig val secret : Security.secret end) = EncodeTransport (Encrypter S)

module CrunchTransport = EncodeTransport Cruncher

module CrunchAndEncryptTransport (S : sig val secret : Security.secret end) (T : Transport) =
	EncryptTransport S (CrunchTransport T)
  
  

(* server side *)	

module type Server =
  sig
  	type global_env
  
  	val become_server     	 : Log.logger -> queue:int -> ?name:string -> global_env -> int -> unit
  	val become_bg_server  	 : Log.logger -> queue:int -> ?name:string -> global_env -> int -> Thread.t
    val connect_and_serve 	 : Log.logger -> global_env -> string -> int -> unit
    val connect_and_bg_serve : Log.logger -> name:string -> global_env -> string -> int -> Thread.t
  end

module MakeServer (A : Api)
				  (I : Interpreter with module Api = A)
				  (F : Format with module Api = A)
				  (T : Transport)
  				  : Server with type global_env = I.global_env
= struct
  	type global_env = I.global_env
  
	exception Cannot_resolve of string
  	exception Connection_refused of string
  
  	let send_ans logger sock ans =
  		logger#msg Log.VeryLow (sprintf ">> %s" (truncate_string (String.escaped (A.pretty_ans ans)) 100));
  		T.send sock (F.marshal_ans ans)
  		
  	let receive_cmd logger sock =
  		let data= T.receive sock
  		in
  			try
  				let cmd = F.unmarshal_cmd data
  				in
  					logger#msg Log.VeryLow (sprintf "<< %s" (truncate_string (String.escaped (A.pretty_cmd cmd)) 100));
					`Wellformed cmd
  			with e ->
  				logger#warn Log.Normal (sprintf "ill-formed command: exception %S caught" (Printexc.to_string e));
  				`Illformed data
			
	let serve logger genv sock =
		let senv = I.init_session logger sock genv
		in			
			(try
				while true do
					let ans =
						match receive_cmd logger sock with
							`Wellformed cmd -> I.interpret_cmd logger sock genv senv cmd
						  | `Illformed cmd  -> I.interpret_illformed_cmd logger sock genv senv cmd
					in
						send_ans logger sock ans
				done
			with Exit 								-> ()
			  |	 End_of_file 
			  |	 Unix.Unix_error (Unix.EPIPE, _, _) -> logger#msg Log.Normal "client disconnected. Finalizing session..."
			  |  e 									-> logger#warn Log.High (sprintf "exception %S caught. Finalizing session..." (Printexc.to_string e))
			  );
			I.finalize_session logger sock genv senv
			
	let become_server logger ~queue ?name global_env port =
		Comm.become_server logger ~queue ?name serve global_env port

	let become_bg_server logger ~queue ?name global_env port =
		Comm.become_bg_server logger ~queue ?name serve global_env port
		
    let connect_and_serve logger global_env addr port =
		let sock = try Comm.connect addr port
				   with Not_found -> raise (Cannot_resolve addr)
  		  			 |  e	      -> raise (Connection_refused (Printexc.to_string e))
  		in
        	serve logger global_env sock
            
    let connect_and_bg_serve logger ~name global_env addr port =
    	let name = sprintf "server(%s)" name
		in
        	XThread.create logger name (fun () -> connect_and_serve logger global_env addr port) ()
            
  end

  
(* client side *)

module type Client =
  sig
  	module Api : Api
  
  	type conn
    
  	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
    
    val connect       : string -> int -> conn
    val accept_server : Log.logger -> int -> conn
	val disconnect    : conn -> unit
	val exec_cmd      : Log.logger -> ?timeout:float -> conn -> Api.cmd -> Api.ans
  end

module MakeClient (A : Api) (F : Format with module Api = A) (T : Transport) : Client with module Api = A =
  struct
  	module Api = A
  
  	type conn = Unix.file_descr XThread.sync

  	exception Cannot_resolve of string
  	exception Connection_refused of string
  	exception Connection_broken
  	exception Connection_timed_out
  	
	let connect addr port =
		let sock = try Comm.connect addr port
				   with Not_found -> raise (Cannot_resolve addr)
  		  			 |  e	      -> raise (Connection_refused (Printexc.to_string e))
  		in
  			new XThread.sync sock
  		
  	let disconnect (conn : conn) = conn#apply Comm.disconnect
  	
    let accept_server logger port =
    	let ssock = Comm.stream_socket ~port () in
    	let (csock, _) =
        	logger#msg Log.Low (sprintf "listening for a server on port %d..." port);
			Unix.listen ssock 1;
			Unix.accept ssock
       	in
        	new XThread.sync csock
    
  	let send_cmd logger sock cmd =
  		let data = F.marshal_cmd cmd
  		in
  			logger#msg Log.VeryLow (sprintf ">> %s" (truncate_string (String.escaped (A.pretty_cmd cmd)) 100));
  			try T.send sock data
  			with e ->
  				logger#warn Log.High (sprintf "exception trapper while sending: %s. Forcing disconnection..." (Printexc.to_string e));
  				Comm.disconnect sock;
  				raise Connection_broken
  		
  	let receive_ans logger ?timeout sock =  			
  		let ans = try T.receive ?timeout sock
  				  with Comm.Receive_timed_out -> Comm.disconnect sock; raise Connection_timed_out
  				    |  e 					  -> logger#warn Log.High (sprintf "exception trapped while receiving: %s. Forcing disconnection..." (Printexc.to_string e));
  				   								 Comm.disconnect sock;
  				   							     raise Connection_broken
  		in
		let ans = F.unmarshal_ans ans
		in
			logger#msg Log.VeryLow (sprintf "<< %s" (truncate_string (String.escaped (A.pretty_ans ans)) 100));
			ans
  
	let exec_cmd logger ?timeout (conn : conn) cmd =
		conn#apply (fun self -> send_cmd logger self cmd; receive_ans logger ?timeout self)
  end
  
  
(* typical clients and servers *)  
	
module MakeClientServer (A : Api) (F : Format with module Api = A) (T : Transport) =
  struct
  	module Server (I : Interpreter with module Api = A) = MakeServer A I F T
  	module Client = MakeClient A F T
  end
	
module MakeBinClientServer (A : Api) (E : Encoder) =
	MakeClientServer A (MakePolyFormat A) (EncodeTransport E RawTransport)
  
module MakeTextClientServer (A : Api)
					 (TF : TextFormat with module Api = A)
					 (T : sig val sep : char val quote : char val term : char end) =
	MakeClientServer A (FormatOfTextFormat TF T) (LineTransport T)







