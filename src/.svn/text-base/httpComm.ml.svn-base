(*
 * HDA Bridge
 *
 * (C) 2007 H-Care srl.
 *)

open Printf
open Prelude


exception Parse_error of string

type meth = Post | Get

let string_of_meth = function Get -> "get" | Post -> "post"

type 'a content_type = Xml | Html | Png | Text | Css | Ico | Custom of 'a

let string_of_content_type custom = function 
	Html 		 -> "text/html; charset=iso-8859-1"
  | Xml 		 -> "application/xml"
  | Png			 -> "image/png"
  | Text		 -> "text/plain"
  | Css			 -> "text/css"
  | Ico			 -> "image/x-icon"
  | Custom x	 -> custom x

type http_request = {
	path	: string list;
	meth	: meth;
	uri		: string;
	params 	: (string, string) Hashtbl.t;
}

type 'a http_response = {
	code			: int;
	reason			: string;
	content_type	: 'a content_type;
	content_length	: int option;
	headers			: (string * string) list;
	body			: [ `Data of string | `Stream of Log.logger -> Comm.socket -> unit ];
}

module type Api = 
  sig
	type cmd
	type ans

	type custom_content_type
	val string_of_custom_content_type : custom_content_type -> string

	val pretty_cmd : cmd -> string
	val pretty_ans : ans -> string

	val cmd_of_http_request : Log.logger -> http_request -> cmd
	val http_response_of_ans : Log.logger -> ans -> custom_content_type http_response

(* La mia risposta ad ocamlnet *)
(*	val http_request_of_cmd : Log.logger -> cmd -> http_request
	val ans_of_http_response : Log.logger -> http_response -> ans  -- per far questo posso usare degli header custom, o l'url *)

	val on_parse_error : string -> cmd	
  end

module type Interpreter =
  sig
	module Api : Api
	val interpret : #Log.logger -> Api.cmd -> [ `Answer of Api.ans 
											  | `Answer_and_do of Api.ans * (unit -> unit) 
											  | `Forward_channel of Log.logger -> CommLib.TCPConnector.channel -> unit ]
	val on_error : exn -> Api.ans
  end

module type Transport =
  sig
	module Api : Api
	val receive_http_request : Log.logger -> CommLib.TCPConnector.channel -> http_request 
	val send_http_response : Log.logger -> CommLib.TCPConnector.channel -> Api.custom_content_type http_response -> unit

(* todo: implementare per avere anche il client *)
(* La mia risposta ad ocamlnet *)
(*	val receive_http_response : CommLib.TCPConnector.channel -> http_response *)
(*	val send_http_request : CommLib.TCPConnector.channel -> http_request -> unit *)
  end

module MakeClientServer 
			(A : Api) 
			(T : Transport with module Api = A) 
			(I : Interpreter with module Api = A) 
			(C : ApiCommLib.TCPServerConfig) =
  struct
	module Conn = CommLib.TCPConnector 
	
	module Server =
	  struct
		let serve (logger : #Log.logger) chan = 
			let send_ans ans = 
				logger#debug (sprintf "!>> %s" (A.pretty_ans ans));
				T.send_http_response logger chan (A.http_response_of_ans logger ans)				
			in
			let cmd = 
			  	try A.cmd_of_http_request logger (T.receive_http_request logger chan)
				with 
					Parse_error s -> A.on_parse_error s
				  | e			  -> A.on_parse_error (sprintf "unexpected exception in parsing request: %s" (pretty_exn e))
			in 
			logger#debug (sprintf "<<? %s" (A.pretty_cmd cmd));
			let ans = 
				try I.interpret logger cmd 
				with e -> `Answer (I.on_error e)
			in
				match ans with 
					`Answer ans 			-> send_ans ans; Conn.disconnect logger () chan
				  | `Answer_and_do (ans, f)	-> send_ans ans; Conn.disconnect logger () chan; f ()
				  | `Forward_channel f		-> f logger chan

		let create_server (logger : #Log.logger) ~name ~address ~port ~queue = 
			let listener_name = 
				let address = match address with 
					"0.0.0.0" 	-> ""
				  | address		-> sprintf "%s:" address
				in
					sprintf "%s:listener@%s%d" name address port in 
			let server_name peer = sprintf "%s:server(%s)" name (Comm.pretty_sockaddr peer) in
			let accept = fun ((_, _, peer) as chan) -> 
				ignore (C.spawn_server 
					logger
					(server_name peer)
					(fun () -> 
					  	try serve logger chan
						with e -> 
						  begin	
							logger#error ~code:[2;2;1]
								(sprintf "exception caught while executing callback: %s. Thread exiting after cleaning up...."
								(Prelude.pretty_exn e));
							Conn.disconnect logger () chan
						  end))
			in
			let listener = fun () ->
				try Conn.become_passive_listener logger (queue, address, port) accept
				with e -> C.on_listener_exn logger e
			in
				ignore (C.spawn_listener logger listener_name listener)
	  end
  end

