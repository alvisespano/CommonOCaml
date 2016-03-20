
open Printf
open XStream

module TA =
  struct
    type cmd = string
  	type ans = string
  	
  	let stream_cmd = stream_ident
  	let stream_ans = stream_ident
  	let parse_cmd = parse_ident
  	let parse_ans = parse_ident
  end
  
module ATF = ApiComm.MakeApiAndTextFormat TA  
  
module I (Ch : Comm.Channel) =
  struct
  	module Api = ATF.Api
    
    type channel = Ch.t
    type global_env = unit
  	type session_env = unit
  	
    let init_session logger genv ch = ()
    let finalize_session logger genv senv ch = ()
    
    let on_exception logger genv senv ch e = ()
    
    let interpret logger genv senv ch s = s
  end  
  
module TCPCS = ApiComm.MakeTextTCPClientServer ATF.Api ATF.TextFormat (struct let quote = '\"' let sep = ' ' let term = '\n' end)
module Server = TCPCS.Server (I TCPCS.Channel)
module Client = TCPCS.Client

let logger = new StdLogger.color_console_logger ()
let port = 1123

;;

if Array.length Sys.argv <> 2 then begin printf "usage: %s <server | client>\n%!" Sys.argv.(0); exit 1 end;
let mode = Sys.argv.(1) in

if mode = "server" then 
    let _ = Server.create_passive_server logger ~name:"prova" ~queue:100 ~port () 
    in
        Unix.pause ()
else if mode = "client" then begin
    let ch = Client.active_connect logger ~addr:"localhost" ~port
    in
        while true do
            printf ">> ";
            let s = read_line ()
            in
                ignore (Client.exec logger ch s);
        done
  end
