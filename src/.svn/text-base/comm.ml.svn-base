(*
 * Common library
 * comm.ml: low-level communication facilities
 *
 * (C) 2005 Quendi srl.
 *)

(** Low-level communication facilities. *)

open Printf
open Prelude
open XString

(* disable sigpipe *)
let _ = Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe]


(*
 * low level interface
 *)

(** Raised when a receive primitive times out. *)
exception Receive_timed_out

(** Raised when a connect primitive times out. *)
exception Connect_timed_out

(** Converts a [Unix.sockaddr] into a pair [(address, port)]. *)
let addr_of_sockaddr = function
	Unix.ADDR_INET (addr, port) -> (addr, port)
  | Unix.ADDR_UNIX _	     	-> raise (Unexpected "Comm.pretty_sockaddr: unix domain socket")
 
(** Pretty prints a [Unix.sockaddr]. *)
let pretty_sockaddr sockaddr =
    let (addr, port) = addr_of_sockaddr sockaddr
    in
        sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

(** Database of all active sockets. *)
let all_sockets =
  object
    val sockets = new XThread.sync (Hashtbl.create 100)

    
    method register sock = sockets#apply 
		(fun tbl -> Hashtbl.add tbl sock (XThread.pretty (Thread.id (Thread.self ())), Time.now ()))
    method unregister id = sockets#apply 
		(fun tbl -> Hashtbl.remove tbl id)
    method fold : 'z. ('z -> 'k -> 'x -> 'z) -> 'z -> 'z = fun f z -> sockets#apply 
		(fun tbl -> Hashtbl.fold (fun id x z -> f z id x) tbl z)

    method length = sockets#apply Hashtbl.length
  end

(** Socket low-level object wrapper. *)
class socket sock =
  object (self) 
    
    initializer all_sockets#register sock
  
    method apply : 'a. (Unix.file_descr -> 'a) -> 'a = fun f -> f sock
  
    method pretty = sprintf "%s%s" (pretty_sockaddr self#getsockname) (try sprintf "<->%s" (pretty_sockaddr self#getpeername) with _ -> "")
  
    method close = all_sockets#unregister sock; Unix.close sock
    method accept = let (sock', sockaddr) = Unix.accept sock in (new socket sock', sockaddr)
    method bind = Unix.bind sock
    method connect = Unix.connect sock
    method listen = Unix.listen sock
    method shutdown = Unix.shutdown sock
    method set_nonblocking = Unix.set_nonblock sock
    method set_blocking = Unix.clear_nonblock sock
    
    method getsockname = Unix.getsockname sock
    method getpeername = Unix.getpeername sock
    
    method getsockopt = Unix.getsockopt sock
    method setsockopt = Unix.setsockopt sock
    method getsockopt_int = Unix.getsockopt_int sock
    method setsockopt_int = Unix.setsockopt_int sock
    method getsockopt_optint = Unix.getsockopt_optint sock
    method setsockopt_optint = Unix.setsockopt_optint sock
    method getsockopt_float = Unix.getsockopt_float sock
    method setsockopt_float = Unix.setsockopt_float sock
   
    method receive_select timeout =
        match Unix.select [sock] [] [] timeout with
    	    ([_], [], []) -> ()
    	  |	([], [], [])  -> raise Receive_timed_out 
    	  | _			  -> raise (Unexpected "Comm.socket#receive_select")
      
    method recv buff pos len = Unix.recv sock buff pos len []
	method recvfrom buff pos len =  Unix.recvfrom sock buff pos len []
    
    method send buff pos len = Unix.send sock buff pos len []
	method sendto buff pos len sockaddr = Unix.sendto sock buff pos len [] sockaddr
    
    method recv_all buff pos len =
    	let rec f pos len =
    		if len > 0 then begin
    			let len' = self#recv buff pos len
    			in
    				if len' = 0 then raise End_of_file
    				else f (pos + len') (len - len')
    		  end
    	in
    		f pos len
		
    method send_all buff pos len =
    	if len > 0 then
    		let len' = self#send buff pos len
    		in
    		    self#send_all buff (pos + len') (len - len')

	method send_string s = self#send_all s 0 (String.length s)

	(** return Some s if a string is in socket before timeout expiration; None if no data available and channel closed *)
	method receive_string ?buffer_size ?timeout () =
		on_some self#receive_select timeout;
		let buffer_size = match buffer_size with None -> 4096 | Some n -> n in
		let buff = String.create buffer_size in
		let received_bytes = self#recv buff 0 buffer_size in 
		let result = String.sub buff 0 received_bytes
		in
			match received_bytes with
				0 	-> None
			  | _	-> Some result

  end
  
  
(* high level interface *)  
(**/**)				
module Int32DumpType =
  struct
  	type t = Int32.t
  	let size = 4
  	let logic_and = Int32.logand
  	let logic_or = Int32.logor
  	let logic_shift_l = Int32.shift_left
  	let logic_shift_r = Int32.shift_right_logical
  	let of_int = Int32.of_int
  	let to_int = Int32.to_int
  end			

(**/**)  
module Int32Dump = NumDump Int32DumpType

(** Sends [data] over socket [sock] in binary mode. A 4-byte header (either in big or little endian depending on [endian])
with the size of [data] is appended before the data itself.  *)
let send_bin ?(endian = `Big_endian) (sock : socket) data =
	let len = String.length data in
	let header = Int32Dump.to_string endian (Int32.of_int len)
	in
		sock#send_all (header ^ data) 0 (4 + len)
		
(** Receives some binary data (according to the format used by [send_bin]) from socket [sock]. Optionally a [timeout] can be specified.
*)	
let receive_bin ?timeout ?(endian = `Big_endian) (sock : socket) =
	let header = String.create 4
	in
	    on_some sock#receive_select timeout;
		sock#recv_all header 0 4;
		let len = Int32.to_int (Int32Dump.of_string endian header) in
		let buff = String.create len
		in
		    sock#recv_all buff 0 len;
			buff

(** Sends string [line] over socket [sock] appending the terminator character [term].
*)
let send_line term (sock : socket) line = sock#send_all (line ^ (String.make 1 term)) 0 (String.length line + 1)

(** Receives a string from socket [sock] until termination character [term] is reached (and discarded). Optionally a [timeout] can be specified.
*)
let receive_line ?timeout term (sock : socket) =
    let c = String.create 1
    and buff = Buffer.create 256 in
	let rec read () =
	    sock#recv_all c 0 1;
		if c.[0] = term then Buffer.contents buff else (Buffer.add_char buff c.[0]; read ())
    in
        on_some sock#receive_select timeout;
		read ()            
	
(** Receives a string from socket [sock] until termination string [term] is reached (and discarded). Optionally a [timeout] can be specified.
*)				
let receive_until ?timeout term (sock : socket) =
    let term_len = String.length term in 
    let buff = Buffer.create 256 in
    let c = String.create 1 in
    let rec read i =
        sock#recv_all c 0 1;
        Buffer.add_char buff c.[0];
        if c.[0] = term.[i] then
            if i < term_len - 1 then read (i + 1)
            else Buffer.sub buff 0 ((Buffer.length buff) - term_len) 
        else read 0
    in
        on_some sock#receive_select timeout;
        read 0		
		
(** Closes TCP/IP socket [sock]. *)
let close_stream (sock : socket) = try sock#close with _ -> ()

(** Creates a socket and optionally binds it to port [port] and to address [address]. If [address] is not specified, it binds to any interface (i.e.: 0.0.0.0). The created socket is a TCP/IP stream socket with KEEPALIVE and REUSEADDR options on
and LINGER off. *)
let open_stream ?(address = "0.0.0.0") ?port () =
	let address = Unix.inet_addr_of_string address in
	let sock = new socket (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)
	in
	    try
        	sock#setsockopt_optint Unix.SO_LINGER None;
	        sock#setsockopt Unix.SO_KEEPALIVE true;
	        on_some (fun port ->
		        sock#setsockopt Unix.SO_REUSEADDR true;
		        sock#bind (Unix.ADDR_INET (address, port))) port;
	        sock
	    with e -> close_stream sock; raise e


	

(** Receives datagram packets from socket [sock] till [len] is reached.
*)
let receive_datagram (sock : socket) len =
	let buf = String.create len in
	let (len, sockaddr) = sock#recvfrom buf 0 len
	in
		(String.sub buf 0 len, sockaddr)

(** Sends datagram packets for string [data] over socket [sock] to address [sockaddr].
*)
let send_datagram (sock : socket) sockaddr data = 
	ignore (sock#sendto data 0 (String.length data) sockaddr)

(** Closes UDP/IP socket [sock]. 
*)
let close_datagram (sock : socket) = try sock#close with _ -> ()
	
(** Creates a socket and optionally binds it to port [port]. The created socket is a UDP/IP datagram socket with REUSEADDR option on
and LINGER off. *)
let open_datagram ?port () =
	let sock = new socket (Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0)
	in
	    try
	        sock#setsockopt_optint Unix.SO_LINGER (Some 0);
        	on_some (fun port ->
        		sock#setsockopt Unix.SO_REUSEADDR true;
        		sock#bind (Unix.ADDR_INET (Unix.inet_addr_any, port))) port;
        	sock
        with e -> close_datagram sock; raise e


(** Creates a TCP/IP socket and connects it to address [addr] and port [port].
*)
let connect ?timeout addr port =
    let conn (sock : socket) addr =
        match timeout with 
            Some timeout ->
                sock#set_nonblocking;
                (try
                    try sock#connect (Unix.ADDR_INET (addr, port))
                    with Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
                        sock#apply (fun fd ->
                            match Unix.select [] [fd] [] timeout with
                                ([], [_], []) -> ()
                              | _             -> raise Connect_timed_out);
                        let err = sock#getsockopt_int Unix.SO_ERROR in
                        if err <> 0 then raise (Failure (sprintf "connect: error code %d" err))
                with e -> sock#set_blocking; raise e);
                sock#set_blocking
                                
          | None -> sock#connect (Unix.ADDR_INET (addr, port))
    in
    	let addrs = Array.to_list ((Unix.gethostbyname addr).Unix.h_addr_list) in
        let rec f sock e = function
            []            -> close_stream sock; raise e
          | addr :: addrs -> try conn sock addr; sock with e -> f sock e addrs
        in
            f (open_stream ()) (Unexpected "Comm.connect: empty address list") addrs
	
(** Shuts down connected socket [sock] on both sides.
*)
let shutdown (sock : socket) = try sock#shutdown Unix.SHUTDOWN_ALL with _ -> ()
	

(** Channel signature. A Channel is an abstraction for sending and receiving data of any type over any kind of link.
[t] is the member type, [send_t] is the type of data sent over the channel, [receive_t] is the type of data received from the channel,
[info_t] is the informational type for the channel. *)
module type Channel =
  sig
    type t
    type send_t
    type receive_t
    type info_t
  
    val send     : #Log.logger -> t -> send_t -> unit
    val receive  : #Log.logger -> ?timeout:float -> t -> receive_t
    val get_info : #Log.logger -> t -> info_t
  end

(** Connector signature. A Connector contains the constructors for Channels and offers the connectivity interface for them.
[channel] is the Channel member type created by the Connector, [active_arg] is the type for auxiliary parameters passed to [active_connect],
[passive_arg] is the type for auxiliary parameters passed to [passive_connect], [listener_arg] is the type for auxiliary parameters passed
to [become_passive_listener], [disconnect_arg] is the type for auxiliary parameters passed to [disconnect].
*)
module type Connector =
  sig
    type channel
  
    type active_arg
    type passive_arg
    type listener_arg
    type disconnect_arg

    (** Creates a channel by connecting actively to a peer. *)
    val active_connect : #Log.logger -> ?timeout:float -> active_arg -> channel
    (** Creates a channel by accepting a connection passively from a peer. *)
    val passive_connect : #Log.logger -> ?timeout:float -> passive_arg -> channel
    (** Accepts connections from peers forever. *)
    val become_passive_listener : #Log.logger -> listener_arg -> (channel -> unit) -> unit
    (** Disconnets a channel. *)
    val disconnect : #Log.logger -> disconnect_arg -> channel -> unit
  end
  
let resolv host =
	match Unix.getaddrinfo host "0" [Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM;] with
		[]		-> None
	  | x :: _	-> 
		  begin
			match x.Unix.ai_addr with
				Unix.ADDR_UNIX _ 				-> raise (Unexpected "resolv ADDR_UNIX")
			  | Unix.ADDR_INET (inet_addr, _)	-> Some (Unix.string_of_inet_addr inet_addr)
		  end

