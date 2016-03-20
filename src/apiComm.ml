(*
 * Common library
 * apiComm.ml: client/server API
 *
 * (C) 2005 Quendi srl.
 *)

(** Client-Server communication facilities. *)

open Prelude
open Printf
open XString
open XStream
open Comm
open CommLib

(** Api signature. An Api is the definition of a command type and an answer type (plus their pretty printers) for the communication between
a client and a server. A client sends commands and receives answers; a server receives commands and sends answers accordingly.
*)
module type Api =
  sig
  	type cmd
  	type ans
  	
  	val pretty_cmd : cmd -> string
  	val pretty_ans : ans -> string
  end
  
(* Format signature. It defines the marshalling and unmarshalling operations for command and answers of Api [Api].
The product of the marshalling can be any type.
*)
module type Format =
  sig
  	module Api : Api
  	
  	type marshalled_cmd
  	type marshalled_ans
  	
  	val marshal_cmd   : Api.cmd -> marshalled_cmd
  	val marshal_ans	  : Api.ans -> marshalled_ans
  	val unmarshal_cmd : marshalled_cmd -> Api.cmd
  	val unmarshal_ans : marshalled_ans -> Api.ans
  end  

(** Interpreter signature. It defines the core of the server side, i.e. the interpreter - basically a function from commands to answers.
*)
module type Interpreter =
  sig
  	module Api : Api
  	
  	(** The channel type. *)
  	type channel
  	
  	(** The global environment type of the server. *)
  	type global_env
  	
  	(** The session environment type of the server. *)
  	type session_env
  	
  	(** Called when an exception occurs either during session initialization or the interpretation loop. *)
  	val on_exception     : #Log.logger -> global_env -> channel -> exn -> [ `Handled | `Unhandled of exn ]
  	
  	(** Called as the channel is created. *)
  	val init_session	 : #Log.logger -> global_env -> channel -> session_env
  	
  	(** Called on interruption of the interpreter or on failure of the channel. *)
  	val finalize_session : #Log.logger -> global_env -> session_env -> channel -> unit
  	
  	(** Called in a loop after session initialization. The [`Answer] variant must be evaluated when a given answer must be replied back to the client;
  	[`Answer_and_do] when an answer must be first sent back to the client and then some operation must be done; `Quiet when no answer must be sent back to the client. *)
  	val interpret   	 : #Log.logger -> global_env -> session_env -> channel -> Api.cmd -> [ `Answer of Api.ans | `Answer_and_do of Api.ans * (unit -> unit) | `Quiet ]
  end
 

(** Configuration signature for servers.
*)	
module type ServerConfig =
  sig
    (** The Connector. *)
    module Connector : Connector
    
    (** The type returned by [spawn_listener]. *)
    type spawn_listener_t
    
    (** The type returned by [spawn_server]. *)
    type spawn_server_t
    
    (** The spawning function for listeners. *)
    val spawn_listener : #Log.logger -> string -> (unit -> unit) -> spawn_listener_t
    
    (** The spawning function for session servers. *)
    val spawn_server : #Log.logger -> string -> (unit -> unit) -> spawn_server_t
    
    (** Evaluates the name of a listener given a prefix. *)
    val listener_name : #Log.logger -> string -> Connector.listener_arg -> string
    
    (** Evaluates the name of  session server given a prefix. *)
    val server_name : #Log.logger -> string -> Connector.channel -> string
    
    (** Called in case a listener fails with an exception. *)
    val on_listener_exn : #Log.logger -> exn -> unit
  end
  
(** Server signature.
*)
module type Server =
  sig
    module Channel : Channel
    
    type global_env

    (** Functor for the basic connectivity, given a Connector and and a ServerConfig. *)
  	module Connectivity : functor (Conn : Connector with type channel = Channel.t) ->
  	                        functor (C : ServerConfig with module Connector = Conn) ->
  	  sig
 	    val create_passive_server : ?inactivity_timeout:float -> #Log.logger -> string -> global_env -> Conn.listener_arg -> Conn.disconnect_arg -> C.spawn_listener_t
        val create_active_server : ?connect_timeout:float -> ?inactivity_timeout:float -> #Log.logger -> string -> global_env -> Conn.active_arg -> Conn.disconnect_arg -> C.spawn_server_t
  	  end
  end


(** MakeServer functor. Creates a Server given an Api, a Channel over commands and answers and an Interpreter.
*)
module MakeServer (A : Api)
                  (Ch : Channel with type send_t = A.ans with type receive_t = A.cmd)
				  (I : Interpreter with module Api = A with type channel = Ch.t)
  : Server with module Channel = Ch with type global_env = I.global_env =
  struct
    module Channel = Ch
    
    type global_env = I.global_env
   
  	let send_ans (logger : #Log.logger) ch ans =
  		logger#debug (sprintf "!>> %s" (truncate_string (String.escaped (A.pretty_ans ans)) 256));
  		Ch.send logger ch ans
  		
  	let receive_cmd ?timeout logger ch =
  	    let cmd = Ch.receive ?timeout logger ch
  		in
  		    logger#debug (sprintf "<<? %s" (truncate_string (String.escaped (A.pretty_cmd cmd)) 256));
			cmd
  	
  	let become_interpreter ?inactivity_timeout logger genv ch =
        let handle_exn e = 
  			match I.on_exception logger genv ch e with
                `Handled     -> ()
              | `Unhandled e -> logger#error ~code:[1;1;4] (sprintf "server fatal exception: %s" (pretty_exn e))
	    in
            try
                let senv =
                    logger#debug "initializing session...";
                    I.init_session logger genv ch
                in			
                    logger#debug "entering interpreter...";
		            (try
				        while true do
                            let cmd = receive_cmd ?timeout:inactivity_timeout logger ch
                            in
                                match I.interpret logger genv senv ch cmd with
                                    `Answer ans             -> send_ans logger ch ans
                                  | `Answer_and_do (ans, f) -> send_ans logger ch ans; f ()
                                  | `Quiet                  -> ()
				        done
			        with Exit                   -> logger#debug "interpreter exited voluntarily"
			          |  Comm.Receive_timed_out -> logger#warn ~code:[1;1;3] "interpreter exited due to inactivity"
			          |  e                      -> handle_exn e
        			  );
                    logger#debug "finalizing session...";
                    I.finalize_session logger genv senv ch
                    
            with e -> handle_exn e
			
			
    module Connectivity (Conn : Connector with type channel = Channel.t) (C : ServerConfig with module Connector = Conn) =
        struct
            let serve ?inactivity_timeout logger name genv ch disconnect_arg =
                C.spawn_server logger (C.server_name logger name ch) (fun () ->
                    become_interpreter logger ?inactivity_timeout genv ch;
                    Conn.disconnect logger disconnect_arg ch)
        
            let create_passive_server ?inactivity_timeout logger name genv listener_arg disconnect_arg =
                C.spawn_listener logger (C.listener_name logger name listener_arg) (fun () ->
                    try Conn.become_passive_listener logger listener_arg (fun ch -> ignore (serve ?inactivity_timeout logger name genv ch disconnect_arg))
                    with e -> C.on_listener_exn logger e)
                 
            let create_active_server ?connect_timeout ?inactivity_timeout logger name genv active_arg disconnect_arg =
                let ch = Conn.active_connect logger ?timeout:connect_timeout active_arg
                in
                    serve ?inactivity_timeout logger name genv ch disconnect_arg
        end			
    
  end


(** Creates a Server given an Api, a Format for that Api and a Channel over the marshalled types.
*)
module MakeFormatServer (A : Api)
                        (F : Format with module Api = A)
                        (Ch : Channel with type send_t = F.marshalled_ans with type receive_t = F.marshalled_cmd)
    = MakeServer A (MapChannel Ch (struct
                                        type send_t' = A.ans
                                        type receive_t' = A.cmd
                                        let map_send = F.marshal_ans
                                        let map_receive = F.unmarshal_cmd
                                     end))
  
(** Configuration signature for client.
@see ServerConfig
*)
module type ClientConfig =
  sig
    module Connector : Connector
    
    type spawn_listener_t
    
    val spawn_listener : #Log.logger -> string -> (unit -> unit) -> spawn_listener_t
    val listener_name : #Log.logger -> string -> Connector.listener_arg -> string
    val on_listener_exn : #Log.logger -> exn -> unit
  end

(** Client signature.
*)
module type Client =
  sig
  	module Api : Api
  	module Channel : Channel
    
    val send    : #Log.logger -> Channel.t -> Api.cmd -> unit
    val receive : #Log.logger -> ?timeout:float -> Channel.t -> Api.ans
	val exec    : #Log.logger -> ?timeout:float -> Channel.t -> Api.cmd -> Api.ans
	
	(** Functor for creating the basic connectivity, given a Connector and a ClientConfig. *)
	module Connectivity : functor (Conn : Connector with type channel = Channel.t) ->
	                        functor (C : ClientConfig with module Connector = Conn) ->
  	  sig
  	    val active_connect : #Log.logger -> ?timeout:float -> Conn.active_arg -> Channel.t
  	    val passive_connect : #Log.logger -> ?timeout:float -> Conn.passive_arg -> Channel.t
 	    val create_passive_listener : #Log.logger -> (Channel.t -> unit) -> string -> Conn.listener_arg -> C.spawn_listener_t
 	    val disconnect : #Log.logger -> Conn.disconnect_arg -> Channel.t -> unit
  	  end
  end
 		
(** Creates a Client given an Api and a Channel over command and answers.
*)
module MakeClient (A : Api) (Ch : Channel with type send_t = A.cmd with type receive_t = A.ans)
  : Client with module Api = A with module Channel = Ch =
  struct
  	module Api = A
  	module Channel = Ch
  	
  	let failed (logger : #Log.logger) s e =
  	    logger#error ~code:[1;1;1] (sprintf "exception caught while %s: %s" s (pretty_exn e));
  	    raise e
  	    
  	let send (logger : #Log.logger) ch cmd =
  	    try
  	        logger#debug (sprintf "?>> %s" (truncate_string (String.escaped (A.pretty_cmd cmd)) 256));
  			Ch.send logger ch cmd
  	    with e -> failed logger "sending command" e
  		    
    let receive logger ?timeout ch =
        try
  		    let ans = Ch.receive logger ?timeout ch
  		    in
  	            logger#debug (sprintf "<<! %s" (truncate_string (String.escaped (A.pretty_ans ans)) 256));
  	            ans
  	    with Comm.Receive_timed_out as e -> raise e
  	       | e                           -> failed logger "receiving answer" e
  	
  	let exec logger ?timeout ch cmd =
  	    send logger ch cmd;
  	    receive logger ?timeout ch
  	 				     
  	module Connectivity (Conn : Connector with type channel = Ch.t) (C : ClientConfig with module Connector = Conn) =
        struct
            let active_connect = Conn.active_connect
        
            let passive_connect = Conn.passive_connect
            
            let create_passive_listener logger f name listener_arg =
                C.spawn_listener logger (C.listener_name logger name listener_arg) (fun () ->
                    try Conn.become_passive_listener logger listener_arg f with e -> C.on_listener_exn logger e)
                    
            let disconnect = Conn.disconnect
        end	 				     
  	 				     
  end
  
(** Creates a Client given an Api, a Format for that Api and a Channel over the marshalled types.
*)
module MakeFormatClient (A : Api)
                        (F : Format with module Api = A)
                        (Ch : Channel with type send_t = F.marshalled_cmd with type receive_t = F.marshalled_ans)
    = MakeClient A (MapChannel Ch (struct
  	                                    type send_t' = A.cmd
                                        type receive_t' = A.ans
  	                                    let map_send = F.marshal_cmd
  	                                    let map_receive = F.unmarshal_ans
  	                                 end))
  
(** Creates both a Client and a Server functor with the same Api given a Channel over commands and answers.
*)
module MakeClientServer (A : Api) =
  struct
    module Server (Ch : Channel with type send_t = A.ans with type receive_t = A.cmd) = MakeServer A Ch
    module Client (Ch : Channel  with type send_t = A.cmd with type receive_t = A.ans) = MakeClient A Ch
  end
  	
(** Creates both a Client and a Server functor with the same Api and the same Format given a Channel over the marshalled_types.
*)  	
module MakeFormatClientServer (A : Api) (F : Format with module Api = A) =
  struct
  	module Server (Ch : Channel with type send_t = F.marshalled_ans with type receive_t = F.marshalled_cmd) = MakeFormatServer A F Ch
    module Client (Ch : Channel with type send_t = F.marshalled_cmd with type receive_t = F.marshalled_ans) = MakeFormatClient A F Ch
  end


