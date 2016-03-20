(*
 * Common daemon library
 * apiCommLib.ml: apiComm library
 *
 * (C) 2007 Alvise Spanò
 *)

(** High-level library for ApiComm. *)

open Prelude
open Printf
open XString
open XStream
open Comm
open CommLib
open ApiComm


(** TextFormat signature. Defines a text-based set of marshallers and unmarshallers for commands and answers.
*)
module type TextFormat =
  sig
  	module Api : Api
  	
  	val stream_cmd : sep:char -> quote:char -> Api.cmd -> string Stream.t
  	val stream_ans : sep:char -> quote:char -> Api.ans -> string Stream.t
  	val parse_cmd  : sep:char -> quote:char -> string Stream.t -> Api.cmd
  	val parse_ans  : sep:char -> quote:char -> string Stream.t -> Api.ans
  end   

(** Creates a Format given a TextFormat and a separator and a quote characters for tokenization.
*)
module FormatOfTextFormat (TF : TextFormat) (S : sig val sep : char val quote : char end)
  : Format with module Api = TF.Api with type marshalled_cmd = string with type marshalled_ans = string =
  struct
  	module Api = TF.Api
  
    type marshalled_cmd = string
  	type marshalled_ans = string
  
  	let marshal_cmd cmd = concat_stream ~sep:S.sep ~quote:S.quote (TF.stream_cmd ~sep:S.sep ~quote:S.quote cmd)
	let marshal_ans ans = concat_stream ~sep:S.sep ~quote:S.quote (TF.stream_ans ~sep:S.sep ~quote:S.quote ans)
	
	let unmarshal parse s =
        let str = try tokenize ~sep:S.sep ~quote:S.quote s
                  with _ -> raise (Failure (sprintf "parse error while tokenizing: %S" s))
        in
            try parse str
            with Stream.Failure                          -> raise (Failure (sprintf "parse error at beginning of input: %S" s))
              |  Stream.Error e when String.length e = 0 -> raise (Failure (sprintf "parse error at end of input: %S" s))
              |  Stream.Error e                          -> raise (Failure (sprintf "parse error at token %S in input: %S" e s))
              |  XStream.Label_not_found lb              -> raise (Failure (sprintf "parse error in input %S: label %s not found" s lb))
              |  e                                       -> raise (Failure (sprintf "parse error in input %S: %s" s (pretty_exn e)))
	
	let unmarshal_cmd s = unmarshal (TF.parse_cmd ~sep:S.sep ~quote:S.quote) s
	let unmarshal_ans s = unmarshal (TF.parse_ans ~sep:S.sep ~quote:S.quote) s
  end


(** Creates a Format for any Api using polymorphic marshallers and unmarshallers and [string] as the marshal product.
*)
module MakePolyFormat (A : Api) : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string =
  struct
  	module Api = A
  
    type marshalled_cmd = string
  	type marshalled_ans = string
  	
  	let marshal_cmd = marsh
  	let marshal_ans = marsh
  	let unmarshal_cmd = unmarsh
  	let unmarshal_ans = unmarsh
  end
  
(** TextApi signature. Defines a text-based compact Api module for defining commands, answers, streamers and parsers.
*)
module type TextApi =
  sig
    type cmd
    type ans
    
    val stream_cmd : sep:char -> quote:char -> cmd -> string Stream.t
  	val stream_ans : sep:char -> quote:char -> ans -> string Stream.t
  	val parse_cmd  : sep:char -> quote:char -> string Stream.t -> cmd
  	val parse_ans  : sep:char -> quote:char -> string Stream.t -> ans
  end
  
(** Creates an Api and a TextFormat given a TextApi. 
*)	
module MakeApiAndTextFormat (A : TextApi) =
  struct
    module Api : Api with type cmd = A.cmd with type ans = A.ans =
      struct
        type cmd = A.cmd
        type ans = A.ans
        
        let pretty_cmd cmd = pretty_stream (A.stream_cmd ~sep:' ' ~quote:'\"' cmd)
        let pretty_ans ans = pretty_stream (A.stream_ans ~sep:' ' ~quote:'\"' ans)
      end
      
    module TextFormat : TextFormat with module Api = Api =
      struct
        module Api = Api
      
        let stream_cmd = A.stream_cmd
        let stream_ans = A.stream_ans
        let parse_cmd = A.parse_cmd
        let parse_ans = A.parse_ans
      end
  end	
	
(**	InterpreterStatus signature. Defines a status type, an initial status and a branch function for interpreters with status.
*)
module type InterpreterStatus =
  sig
    module Api : Api
    
    type t
    
    val initial : t
    
    (** Evaluates what to do given a command and a status. [`Status] sets a new status, [`Exit] makes the interpreter quit, [`Answer_and_exit]
    makes the interpreter reply back to the client and then quit. *)
    val branch  : #Log.logger -> Api.cmd -> t -> [`Status of t | `Exit | `Answer_and_exit of Api.ans ]
  end	
	
(**	Creates an interpreter with status given an Interpreter and an InterpreterStatus for the same Api.
*)
module MakeStatefulInterpreter (I : Interpreter) (S : InterpreterStatus with module Api = I.Api) =
  struct
    module Api = I.Api
  	
  	type channel = I.channel
  	type global_env = I.global_env
  	type session_env = S.t ref * I.session_env
  	
  	let on_exception = I.on_exception
  	
    let init_session logger genv ch =
  	    let senv = I.init_session logger genv ch
  	    in
  	        (ref S.initial, senv)
  	
  	let finalize_session logger genv (_, senv) ch = I.finalize_session logger genv senv ch
  	
  	let interpret logger genv (st, senv) ch cmd =
  	    match S.branch logger cmd !st with
  	        `Status st'          -> st := st'; I.interpret logger genv senv ch cmd
  	      | `Exit                -> raise Exit
      	  | `Answer_and_exit ans -> `Answer_and_do (ans, fun () -> raise Exit)
  end
	
	
	
(** Configuration for TCPServers. *)	
module type TCPServerConfig =
  sig
    type spawn_listener_t
    type spawn_server_t
    
    val spawn_listener : #Log.logger -> string -> (unit -> unit) -> spawn_listener_t
    val spawn_server : #Log.logger -> string -> (unit -> unit) -> spawn_server_t
    val on_listener_exn : #Log.logger -> exn -> unit
  end
	
(** Servers over TCPChannels and TCPConnectors. *)
module type TCPServer =
  sig
    module Channel : TCPChannel
  
  	type global_env
  
  	module Connectivity : functor (Conn : TCPConnector with type channel = Channel.t) ->
  	                        functor (C : TCPServerConfig) ->
  	  sig
 	    val create_passive_server : ?inactivity_timeout:float -> #Log.logger -> name:string -> queue:int -> address:string -> port:int -> global_env -> C.spawn_listener_t
        val create_active_server : ?connect_timeout:float -> ?inactivity_timeout:float -> #Log.logger -> name:string -> addr:string -> port:int -> global_env -> C.spawn_server_t
  	  end
  end	
	
(** Configuration for TCPClientConfig. *)
module type TCPClientConfig =
  sig
    type spawn_listener_t
    
    val spawn_listener : #Log.logger -> string -> (unit -> unit) -> spawn_listener_t
    val on_listener_exn : #Log.logger -> exn -> unit
  end	
	
(** Clients over TCPChannels and TCPConnectors. *)
module type TCPClient =
  sig
    module Api : Api
  	module Channel : TCPChannel
    
    val send    : #Log.logger -> Channel.t -> Api.cmd -> unit
    val receive : #Log.logger -> ?timeout:float -> Channel.t -> Api.ans
	val exec    : #Log.logger -> ?timeout:float -> Channel.t -> Api.cmd -> Api.ans
	
	module Connectivity : functor (Conn : TCPConnector with type channel = Channel.t) ->
	                      functor (C : TCPClientConfig) ->
  	  sig
  	    val active_connect : ?timeout:float -> #Log.logger -> addr:string -> port:int -> Channel.t
  	    val passive_connect : ?timeout:float -> #Log.logger -> port:int -> Channel.t
 	    val create_passive_listener : #Log.logger -> (Channel.t -> unit) -> name:string -> queue:int -> address:string -> port:int -> C.spawn_listener_t
 	    val disconnect : #Log.logger -> Channel.t -> unit
  	  end
  end	

(** Creates a TCPServer given an Api, a Format on that Api, a TCPChannel and an Interpreter.
*)
module MakeTCPServer (A : Api)
                     (F : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string)
                     (Ch : TCPChannel)
                     (I : Interpreter with module Api = A with type channel = Ch.t)
: TCPServer with module Channel = Ch with type global_env = I.global_env
= struct
    module S = MakeFormatServer A F Ch (struct
        module Api = A
 	    type channel = I.channel
        type global_env = I.global_env
    	type session_env = I.session_env
	
        let on_exception logger genv ch e =
            match I.on_exception logger genv ch e with
                  `Unhandled End_of_file              -> logger#info "connection shut down by peer"; `Handled
                | `Unhandled (Unix.Unix_error _ as e) -> logger#error ~code:[1;1;5] (sprintf "interpreter fatal exception: %s" (pretty_exn e)); `Handled
                | r                                   -> r
            
        let init_session = I.init_session
        let finalize_session = I.finalize_session
        let interpret = I.interpret
      end)
    
    module Channel = Ch
    
    type global_env = S.global_env

    module Connectivity (Conn : TCPConnector with type channel = Channel.t) (Conf : TCPServerConfig) =
      struct
        module Conf =
          struct
            module Connector = Conn
   
            type spawn_listener_t = Conf.spawn_listener_t
            type spawn_server_t = Conf.spawn_server_t
    
            let spawn_listener = Conf.spawn_listener
            let spawn_server = Conf.spawn_server
            let listener_name _ name (_, address, port) = 
				let address = match address with 
					"0.0.0.0" 	-> ""
				  | address		-> sprintf "%s:" address
				in
					sprintf "%s:listener@%s%d" name address port
            let server_name logger name ch = sprintf "%s:server(%s)" name (Comm.pretty_sockaddr (snd (Ch.get_info logger ch)))
            let on_listener_exn = Conf.on_listener_exn
          end
          
        module C = S.Connectivity Conn Conf
            
        let create_passive_server ?inactivity_timeout logger ~name ~queue ~address ~port genv =
            C.create_passive_server ?inactivity_timeout logger name genv (queue, address, port) ()
            
        let create_active_server ?connect_timeout ?inactivity_timeout logger ~name ~addr ~port genv =
            C.create_active_server ?connect_timeout ?inactivity_timeout logger name genv (addr, port) ()
      end	
  end
  
(** Creates a TCPClient given an Api, a Format on that Api and a TCPChannel.
*)  	
module MakeTCPClient (A : Api)
                     (F : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string)
                     (Ch : TCPChannel)
: TCPClient with module Channel = Ch with module Api = A
= struct
    module C = MakeFormatClient A F Ch
    
    module Api = A
    module Channel = Ch
    
    let send = C.send
    let receive = C.receive
    let exec = C.exec
  
    module Connectivity (Conn : TCPConnector with type channel = Channel.t) (Conf : TCPClientConfig) =
      struct
        module Conf =
          struct
            module Connector = Conn
   
            type spawn_listener_t = Conf.spawn_listener_t
            
            let spawn_listener = Conf.spawn_listener
            let listener_name _ name (_, address, port) = 
				let address = match address with 
					"0.0.0.0" 	-> ""
				  | address		-> sprintf "%s:" address
				in
					sprintf "%s:listener@%s%d" name address port

            let on_listener_exn = Conf.on_listener_exn
          end
        module C = C.Connectivity Conn Conf
        
        let active_connect ?timeout logger ~addr ~port = C.active_connect logger ?timeout (addr, port)
        let passive_connect ?timeout logger ~port = C.passive_connect logger ?timeout port
        let create_passive_listener logger f ~name ~queue ~address ~port = C.create_passive_listener logger f name (queue, address, port)
        let disconnect logger = C.disconnect logger ()
      end  	
  end
  	
(** Creates both a TCPServer and a TCPClient with the same Api and Format.
*)  	
module MakeTCPClientServer (A : Api) (F : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string) =
  struct
  	module Server (Ch : TCPChannel) = MakeTCPServer A F Ch
    module Client (Ch : TCPChannel) = MakeTCPClient A F Ch
  end

(** Creates both a TCPServer and a TCPClient with the same Api and polymorphic marshallers.
*)
module MakePolyTCPClientServer (A : Api) = MakeTCPClientServer A (MakePolyFormat A)
  
(** Creates both a TCPServer and a TCPClient with the same Api and TextFormat.
*)
module MakeTextTCPClientServer (A : Api) (TF : TextFormat with module Api = A) (T : sig val sep : char val quote : char end) =
	MakeTCPClientServer A (FormatOfTextFormat TF T)
(** Creates both a TCPServer and a TCPClient with the same Api and TextFormat on a LineChannel.
*)
module MakeLineTextTCPClientServer (A : Api)
                                   (TF : TextFormat with module Api = A)
                                   (T : sig val sep : char val quote : char val term : char end)
= struct
    module CS = MakeTextTCPClientServer A TF T
  	module Server (ChF : functor (T : sig val term : char end) -> TCPChannel) = CS.Server (ChF T)
    module Client (ChF : functor (T : sig val term : char end) -> TCPChannel) = CS.Client (ChF T)
  end

(** Dummy TCPServerConfig for quick usage. *)
module DummyTCPServerConfig =
  struct
    type spawn_listener_t = unit
    type spawn_server_t = unit
    let on_listener_exn _ _ = raise (Unexpected "DummyTCPConfig.on_listener_exn")
    let spawn_listener _ _ = raise (Unexpected "DummyTCPSConfig.spawn_listener")
    let spawn_server _ _ _ = raise (Unexpected "DummyTCPSConfig.spawn_server")
  end

(** Dummy TCPClientConfig for quick usage. *)
module DummyTCPClientConfig : TCPClientConfig = DummyTCPServerConfig


(** Handshake signature. Handshaking is handled as an Api plus a set of functions for manually sending and receiving commands and answers.
*)
module type Handshake =
  sig
    module HApi : Api
    type channel
    val send_handshake_ans : #Log.logger -> channel -> HApi.ans -> unit
    val send_handshake_cmd : #Log.logger -> channel -> HApi.cmd -> unit
    val receive_handshake_cmd : #Log.logger -> ?timeout:float -> channel -> HApi.cmd
    val receive_handshake_ans : #Log.logger -> ?timeout:float -> channel -> HApi.ans
  end

(** Creates an Handshake with a given Api, Format and TCPChannel.
*)
module MakeHandshake (A : Api)
                     (F : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string)
                     (Ch : TCPChannel)
: Handshake with type channel = Ch.t with module HApi = A
= struct
    module HApi = A
    type channel = Ch.t
    let send_handshake_ans logger ch ans = Ch.send logger ch (F.marshal_ans ans)
    let send_handshake_cmd logger ch cmd = Ch.send logger ch (F.marshal_cmd cmd)
    let receive_handshake_cmd logger ?timeout ch = F.unmarshal_cmd (Ch.receive ?timeout logger ch)
    let receive_handshake_ans logger ?timeout ch = F.unmarshal_ans (Ch.receive ?timeout logger ch)
  end
  
  
(** Creates both a TCPServer and a TCPClient with the given TextApi with separator [whitespace] and quoter [quotation] over a LineChannel
with terminator [\n].
*)
module LineTextTCPClientServer (TA : TextApi) =
  struct
    module T = struct let sep = ' ' let quote = '\"' let term = '\n' end
    module ATF = MakeApiAndTextFormat TA
    module M = MakeLineTextTCPClientServer ATF.Api ATF.TextFormat T
    include M
    module Api = ATF.Api
  end  

(** Creates both a TCPServer and a TCPClient with the given TextApi and an additional TextApi for the handshake over a LineChannel
with separator [whitespace], quoter [quotation] and terminator [\n].
*)
module LineTextTCPClientServerWithHandshake (TA : TextApi) (HTA : TextApi) =
  struct
    module M = LineTextTCPClientServer TA
    module HATF = MakeApiAndTextFormat HTA
    module H (Ch : TCPChannel) = MakeHandshake HATF.Api (FormatOfTextFormat HATF.TextFormat M.T) Ch

    module Server (ChF : functor (T : sig val term : char end) -> TCPChannel)
                  (I : functor (Ch : TCPChannel with type t = ChF(M.T).t) ->
                           functor (H : Handshake with module HApi = HATF.Api with type channel = Ch.t) ->
                               Interpreter with module Api = M.ATF.Api with type channel = Ch.t) = M.Server ChF (I (ChF M.T) (H (ChF M.T)))

    module Client (ChF : functor (T : sig val term : char end) -> TCPChannel) =
      struct
        module C = M.Client ChF
        module H = H (ChF M.T)
        include C
        include H
      end
      
    module Api = M.ATF.Api
    module HApi = HATF.Api
  end

(** Creates both a TCPServer and a TCPClient with the given Api and Format and an additional Api and Format for the handshake over a TCPChannel.
The TCPChannel can be filtered by the functor [ChF].
*)
module TCPClientServerWithHandshake (A : Api)
                                    (HA : Api)
                                    (F : Format with module Api = A with type marshalled_cmd = string with type marshalled_ans = string)
                                    (HF : Format with module Api = HA with type marshalled_cmd = string with type marshalled_ans = string)
                                    (ChF : functor (Ch : TCPChannel) -> TCPChannel) =
  struct
    module M = MakeTCPClientServer A F
    module H (Ch : TCPChannel) = MakeHandshake HA HF Ch
    
    module Server (Ch : TCPChannel)
                  (I : functor (Ch' : TCPChannel with type t = ChF(Ch).t) ->
                          functor (H : Handshake with module HApi = HA with type channel = Ch'.t) ->
                              Interpreter with module Api = A with type channel = Ch'.t) =
        struct
            module Ch = ChF Ch
            module S : TCPServer with module Channel = Ch with type global_env = (I(Ch)(H(Ch))).global_env = M.Server Ch (I Ch (H Ch))
            include S        
        end
    
    module Client (Ch : TCPChannel) =
      struct
        module C = M.Client (ChF Ch)
        module H = H (ChF Ch)
        include C
        include H
      end

    module Api = A
    module HApi = HA
  end
  

