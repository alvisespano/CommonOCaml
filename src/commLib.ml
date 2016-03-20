(*
 * Common library
 * commLib.ml: comm library
 *
 * (C) 2005 Quendi srl.
 *)
 
(** High-level library for Comm. *)

open Printf
open Prelude
open XString
open Comm


(** Creates a synchronized Channel and Connector pair given any Channel and Connector pair.
Synchronization is one for both directions of the channel, i.e. when the channel is locked for receving data, it is locked for sending as well.
*)
module SynchronizeSimplex (Ch : Channel) (Conn : Connector with type channel = Ch.t) =
  struct
    module Connector =
      struct
        type channel = Conn.channel * unit XThread.sync
        type active_arg = Conn.active_arg
        type passive_arg = Conn.passive_arg
        type listener_arg = Conn.listener_arg
        type disconnect_arg = Conn.disconnect_arg
        
        let create ch = (ch, new XThread.sync ())
        
        let active_connect logger ?timeout arg = create (Conn.active_connect logger ?timeout arg)
        let passive_connect logger ?timeout arg = create (Conn.passive_connect logger ?timeout arg)
        let disconnect logger arg ch = ch#apply (Conn.disconnect logger arg)
        let become_passive_listener logger arg f = Conn.become_passive_listener logger arg (fun ch -> f (create ch))
      end
      
    module Channel =
      struct
        type t = Connector.channel
        type send_t = Ch.send_t
        type receive_t = Ch.receive_t
        type info_t = Ch.info_t
    
      	let send logger ((ch, sy) : t) s = sy#apply (fun () -> Ch.send logger ch s)
  	  	let receive logger ?timeout ((ch, sy) : t) = sy#apply (fun () -> Ch.receive logger ?timeout ch)
  	    let get_info logger ((ch, _) : t) = Ch.get_info logger ch
      end
  end

(** Creates a synchronized Channel and Connector pair given any Channel and Connector pair.
Synchronization is dual for the two directions of the channel, i.e. when the channel is locked for receving data, it can be used for sending.
*)
module SynchronizeDuplex (Ch : Channel) (Conn : Connector with type channel = Ch.t) =
  struct
    module Connector =
      struct
        type channel = Conn.channel * unit XThread.sync * unit XThread.sync
        type active_arg = Conn.active_arg
        type passive_arg = Conn.passive_arg
        type listener_arg = Conn.listener_arg
        type disconnect_arg = Conn.disconnect_arg
        
        let create ch = (ch, new XThread.sync (), new XThread.sync ())
        
        let active_connect logger ?timeout arg = create (Conn.active_connect logger ?timeout arg)
        let passive_connect logger ?timeout arg = create (Conn.passive_connect logger ?timeout arg)
        let disconnect logger arg (ch, ssy, rsy) = ssy#apply (fun () -> rsy#apply (fun () -> Conn.disconnect logger arg ch))
        let become_passive_listener logger arg f = Conn.become_passive_listener logger arg (fun ch -> f (create ch))
      end
      
    module Channel =
      struct
        type t = Connector.channel
        type send_t = Ch.send_t
        type receive_t = Ch.receive_t
        type info_t = Ch.info_t
    
      	let send logger ((ch, ssy, _) : t) s = ssy#apply (fun () -> Ch.send logger ch s)
  	  	let receive logger ?timeout ((ch, _, rsy) : t) = rsy#apply (fun () -> Ch.receive logger ?timeout ch)
  	    let get_info logger ((ch, _, _) : t) = Ch.get_info logger ch
      end
  end

(** Maps a Channel with the given functions for mapping send and receive types and sent and received datas.
The resulting Channel has the same characteristics of the one in input.
*)
module MapChannel (Ch : Channel)
                  (M : sig
                        type send_t'
                        type receive_t'
                        val map_send : send_t' -> Ch.send_t
                        val map_receive : Ch.receive_t -> receive_t'
                       end)
= struct
    type t = Ch.t
    type send_t = M.send_t'
    type receive_t = M.receive_t'
    type info_t = Ch.info_t
    
    let send logger ch x = Ch.send logger ch (M.map_send x)
    let receive logger ?timeout ch = M.map_receive (Ch.receive logger ?timeout ch)
    let get_info = Ch.get_info
  end

(** Creates a Channel given any Channel and a pair for functions for processing sent and received data. Type do not change.
*)
module EncodeChannel (Ch : Channel)
                     (E : sig
                            val encode : Ch.send_t -> Ch.send_t
                            val decode : Ch.receive_t -> Ch.receive_t
                          end)
    : Channel with type send_t = Ch.send_t with type receive_t = Ch.receive_t with type t = Ch.t with type info_t = Ch.info_t
    = MapChannel Ch (struct
            type send_t' = Ch.send_t
            type receive_t' = Ch.receive_t
            let map_send = E.encode
            let map_receive = E.decode
          end)
    
(** Encode a Channel through a data compressor.
*)    
module CrunchChannel (Ch : Channel with type send_t = string with type receive_t = string) =
    EncodeChannel Ch (struct
        open Security
        let encode s = marsh (crunch s)
  	    let decode s = decrunch (unmarsh s)
  	  end)

(** Encode a Channel through a data encrypter, given a secret.
*) 
module EncryptChannel (S : sig val secret : Security.secret end) (Ch : Channel with type send_t = string with type receive_t = string) =
    EncodeChannel Ch (struct
 	    open Security
  	    let encode s = marsh (encrypt S.secret s)
  	    let decode s = decrypt S.secret (unmarsh s)
    end)


(* 
 * channel and connector spooler
 *)

(*
module Multiplex (Ch : Channel)
                 (M : sig
                        val marshal_send : int * Ch.send_t -> Ch.send_t
                        val unmarshal_receive : Ch.receive_t -> int * Ch.receive_t
                      end)
                 (Conn : Connector with type channel = Ch.t) =
  struct
    module Connector =
      struct
        type receiver_pool =  [`Data of Ch.receive_t | `Exn of exn | `Timed_out] XThread.pool
      
        type channel = { channel        : Conn.channel;
                         receiver_pool  : receiver_pool;
                         sender_pool    : [`Send of int * Ch.send_t | `Quit] XThread.pool;
                         receiver_pools : (int, receiver_pool) Hashtbl.t }
                         
        type active_arg = Conn.channel
        type passive_arg = Conn.passive_arg
        type listener_arg = Conn.listener_arg
        type disconnect_arg = Conn.disconnect_arg
        
        let create_sub ch = 
                let rpool = new XThread.pool in
                Hashtbl.add ch.receiver_pools (Oo.id rpool) rpool;
                { ch with receiver_pool = rpool }
                
        let create_real logger ch =
            let sender_pool = new XThread.pool in
            let receiver_pools = Hashtbl.create 100 in
            let sender () =
                try
                    while true do
                        match sender_pool#acquire with
                            `Send (sub, x) -> Ch.send logger ch (M.marshal_send (sub, x))
                          | `Quit          -> raise Exit
                    done
                with Exit -> ()
            in
            let receiver () =
                try
                    while true do
                        let (sub, x) = M.unmarshal_receive (Ch.receive logger ch) in
                        let rpool = Hashtbl.find receiver_pools sub
                        in
                            rpool#release (`Data x) 
                    done
                with e -> Hashtbl.iter (fun _ rpool -> rpool#release (`Exn e)) receiver_pools
            in
            let _ = XThread.create logger "channelreceiver" receiver ()
            and _ = XThread.create logger "channelsender" sender ()
            in
            let rpool = new XThread.pool
            in
                Hashtbl.add receiver_pools (Oo.id rpool) rpool;
                { channel = ch; receiver_pool = rpool ; sender_pool = sender_pool; receiver_pools = receiver_pools }
            
        let active_connect logger ?timeout = function
            `Real arg   -> create_real logger (Conn.active_connect logger ?timeout arg)
          | `Virtual ch -> create_sub ch
        
        let passive_connect logger ?timeout = function
            `Real arg   -> create_real logger (Conn.passive_connect logger ?timeout arg)
          | `Virtual ch -> create_sub ch
        
        let disconnect logger arg ch =
            match arg with
                `Real arg   -> Conn.disconnect logger arg ch.channel
              | `Virtual    -> ()
        
        let become_passive_listener logger arg f =
            match Conn.become_passive_listener logger arg (fun ch -> f (create ch)) with
        
      end

    module Channel =
      struct
        type t = Connector.channel
        type send_t = Ch.send_t
        type receive_t = Ch.receive_t
        type info_t = Ch.info_t * int
    
      	let send _ ch x = ch.sender_pool#release (`Send (ch.sub, x))
      	
        let receive _ ?timeout ch =
            let f  () =
                match (Hashtbl.find receive_pools ch.sub)#acquire with
                      `Exn e     -> raise e
                    | `Data x    -> x
                    | `Timed_out -> raise Comm.Receive_timed_out
            in
                match timeout with
                    None         -> f ()
                  | Some timeout ->
                        manage_resource
                            (fun () -> Timer.add_handler logger ~interval:timeout Timer.Once (fun _ -> self#release `Timed_out))
                            (fun _ -> f ())
                            (fun tid -> Timer.remove_handler tid)
            
  	    let get_info logger ch = (Ch.get_info logger ch.channel, sub)
  	  end
  end
*)
  


(** TCPChannel signature. Refinement of Channel with send and receive types set to string and info type to a pair [Unix.sockaddr] (the local
sockaddr and the peer sockaddr).
*)
module type TCPChannel = Channel with type info_t = Unix.sockaddr * Unix.sockaddr
                                 with type send_t = string with type receive_t = string

(** TCPConnector signature. Refinement of Connector with [active_arg] set to (address, port) pair, [passive_arg] set to port and [listener_arg]
set to (listen queue, address, port) triple.
*)
module type TCPConnector = Connector with type active_arg = string * int
                                     with type passive_arg = int 
									 with type listener_arg = int * string * int
                                     with type disconnect_arg = unit

(** TCPConnector implementation for TCPChannels.
The [channel] type reveals the triple (socket, local sockaddr, peer sockaddr).
*)
module TCPConnector : TCPConnector with type channel = Comm.socket * Unix.sockaddr * Unix.sockaddr =
  struct
    type channel = Comm.socket * Unix.sockaddr * Unix.sockaddr
    
    type active_arg = string * int
    type passive_arg = int
    type listener_arg = int * string * int
    type disconnect_arg = unit

    let create_channel sock ?(peer = sock#getpeername) () = (sock, sock#getsockname, peer)
    
    (* let active_connect logger ?timeout (addr, port) =
        try
            let sock = connect ?timeout addr port
            in
                logger#info Log.Low (sprintf "connected to %s:%d" addr port);
                create_channel sock ()
        with Not_found as e -> logger#info Log.Normal (sprintf "cannot resolve host %s" addr); raise e
           | e              -> logger#info Log.Normal (sprintf "cannot connect to host %s:%d: %s" addr port (pretty_exn e)); raise e *)

    let active_connect (logger : #Log.logger) ?timeout (addr, port) =
        let sock = connect ?timeout addr port
        in
            logger#debug (sprintf "connected to %s:%d" addr port);
            create_channel sock ()
    
    let passive_connect _ ?timeout port =
        match timeout with 
            Some _ -> raise (Unexpected "Comm.TCPConnector.passive_connect: timeout not implemented")
          | None   ->
                let ssock = open_stream ~port () in
                let (csock, csockaddr) =
                    (* logger#info (sprintf "listening on port %d..." port); *)
        			ssock#listen 1;
        			ssock#accept
        		in
        		    close_stream ssock;
        		    create_channel ~peer:csockaddr csock ()
    
    let disconnect (logger : #Log.logger) () ((sock, _, peer) : channel) =
        logger#debug (sprintf "disconnecting from peer %s" (pretty_sockaddr peer));
        shutdown sock;
        close_stream sock
    
    let become_passive_listener (logger : #Log.logger) (queue, address, port) f =
        let ssock = open_stream ~address ~port ()
        in
            try
                logger#info (sprintf "listener up on port %d" port);
    		    while true do
    			    let (csock, csockaddr) = 
	                    (* logger#debug (sprintf "listening on port %d..." port); *)
		      		    ssock#listen queue;
		    		    ssock#accept
	                in
    			        try
    			            logger#debug  (sprintf "accepted incoming connection from peer %s" (pretty_sockaddr csockaddr));
    			            f (create_channel ~peer:csockaddr csock ())
   			            with Exit -> close_stream csock; raise Exit
   			              |  e    -> logger#error ~code:[1;1;2] (sprintf "exception caught: %s" (pretty_exn e));
   			                         close_stream csock
    		    done
            with Exit -> logger#info "passive listener exited voluntarily"
              |  e    -> close_stream ssock; raise e
  end
  
(** Creates a TCPChannel given a send and a receive function over sockets.
*)
module MakeTCPChannel (T : sig
                             val send : #Log.logger -> socket -> string -> unit
                             val receive : #Log.logger -> ?timeout:float -> socket -> string
                           end)
: TCPChannel with type t = TCPConnector.channel
= struct
    type t = TCPConnector.channel
    type send_t = string
    type receive_t = string
    type info_t = Unix.sockaddr * Unix.sockaddr
    
  	let send logger ((sock, _, _) : t) s = T.send logger sock s
  	let receive logger ?timeout ((sock, _, _) : t) = T.receive logger ?timeout sock
  	let get_info _ ((_, local, peer) : t) = (local, peer)
  end

(** Creates a TCPChannel over on a line-based transport with the given terminator.
*)
module LineTCPChannel (T : sig val term : char end) =
    MakeTCPChannel (struct
                    let send _ sock s =
                        let s = escape [T.term] s
                        in
                            send_line T.term sock s
                        
                    let receive _ ?timeout sock =
                        let s = receive_line ?timeout T.term sock 
                        in
                            unescape [T.term] s
                   end)

(** TCPChannel with binary transport.
*)
module BinTCPChannel =
    MakeTCPChannel (struct
                    let send _ sock s = send_bin sock s
                    let receive _ ?timeout sock = receive_bin ?timeout sock
                   end)
  
  
  
(** ConnectorStats signature. Refinement of Connector with statistics over channels.
*)
module type ConnectorStats =
  sig
  	include Connector
	val channels : #Log.logger -> channel list
  end

module MakeConnStats (Ch : Channel) (ChStats : sig type t end) = 
  struct
	let channels : (Ch.t, ChStats.t) Hashtbl.t XThread.sync = new XThread.sync (Hashtbl.create 10)

	let get _ = 
		let f k x acc = (k, x) :: acc
		in
			channels#apply (fun channels -> Hashtbl.fold f channels [])
	let add _ (ch, stats) = channels#apply (fun channels -> Hashtbl.add channels ch stats)
	let remove _ (ch, _) = channels#apply (fun channels -> Hashtbl.remove channels ch) 	
  end

module MakeChStats (Ch : Channel) 
					    (T : sig
								val size_of_send_t    : Ch.send_t -> int
								val size_of_receive_t : Ch.receive_t -> int
							 end)
= struct
	type t = (int * int) XThread.sync

	let create _ = new XThread.sync (0, 0) (* received bytes, sent bytes *)
	let reg_send _ t send_t = t#apply_and_set (fun (input, output) -> (input, output + (T.size_of_send_t send_t)))
	let reg_receive _ t receive_t = t#apply_and_set (fun (input, output) -> (input + (T.size_of_receive_t receive_t), output))
  end

module MakeTCPChStats (Ch : TCPChannel) = 
			MakeChStats Ch (struct  
								let size_of_send_t = String.length
								let size_of_receive_t = String.length
							end)

module MakeStats (Conn : Connector) 
				 (Ch : Channel with type t = Conn.channel)
				 (ChStats :   sig
									type t
									val create		: #Log.logger -> t
									val reg_send	: #Log.logger -> t -> Ch.send_t -> unit
									val reg_receive : #Log.logger -> t -> Ch.receive_t -> unit
						      end)
				 (ConnStats : sig
	                           		val get    : #Log.logger -> (Ch.t * ChStats.t) list
									val add    : #Log.logger -> (Ch.t * ChStats.t) -> unit
									val remove : #Log.logger -> (Ch.t * ChStats.t) -> unit
	                          end)	
: sig 
	module Channel : Channel 
		with type t = Ch.t * ChStats.t 
		with type info_t = Ch.info_t * ChStats.t
		with type receive_t = Ch.receive_t
		with type send_t = Ch.send_t
	module Connector : ConnectorStats 
		with type channel = Channel.t 
	    with type disconnect_arg = Conn.disconnect_arg
		with type listener_arg = Conn.listener_arg
		with type active_arg = Conn.active_arg
		with type passive_arg = Conn.passive_arg
  end 
= struct
	module Channel =
  	  struct
		type t = Ch.t * ChStats.t
	    type send_t = Ch.send_t
	    type receive_t = Ch.receive_t
	    type info_t = Ch.info_t * ChStats.t
	  
	    let send logger ((t, stats) : t) (x : send_t) =
			ChStats.reg_send logger stats x;
			Ch.send logger t x

		let receive logger ?timeout ((t, stats) : t) =
			let x = Ch.receive logger ?timeout t 
			in
				ChStats.reg_receive logger stats x;
				x

	    let get_info logger (t, stats) : info_t = (Ch.get_info logger t, stats)
	  end

	module Connector =
	  struct
		type channel = Channel.t
	  
	    type active_arg = Conn.active_arg
	    type passive_arg = Conn.passive_arg
	    type listener_arg = Conn.listener_arg
	    type disconnect_arg = Conn.disconnect_arg

		let channels logger = ConnStats.get logger

		let active_connect logger ?timeout active_arg = 
			let ch = Conn.active_connect logger ?timeout active_arg in	
			let stats = ChStats.create logger
			in
				ConnStats.add logger (ch, stats);
				(ch, stats)

		let passive_connect logger ?timeout passive_arg = 
			let ch = Conn.passive_connect logger ?timeout passive_arg in
			let stats = ChStats.create logger
			in	
				ConnStats.add logger (ch, stats);
				(ch, stats)

		let become_passive_listener logger listener_arg f =
			let f' ch =
				let stats = ChStats.create logger
				in
					ConnStats.add logger (ch, stats);
					f (ch, stats)
			in
				Conn.become_passive_listener logger listener_arg f'

		let disconnect logger (disconnect_arg : disconnect_arg) (ch : channel) =
			ConnStats.remove logger ch;
			Conn.disconnect logger disconnect_arg (fst ch)
	  end    
  end

(*
esempio statistiche sul canale 
*)
(*
module C = CommLib.MakeStats
				CommLib.TCPConnector
				Channel_
				(CommLib.MakeTCPChStats Channel_)
				(CommLib.MakeConnStats Channel_ (CommLib.MakeTCPChStats Channel_))

module Channel = C.Channel
module Connector = C.Connector
*)


