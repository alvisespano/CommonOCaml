
open Printf


module A =
  struct
    type cmd = Ou
  	type ans = Eh
  	
  	let pretty_cmd = function Ou -> "ou"
  	let pretty_ans = function Eh -> "eh"
  end
  
module I =
  struct
  	module Api = A
    
    type global_env = unit
  	type session_env = unit
  	
    let init_session logger genv = ()
    let finalize_session logger genv senv = ()
    
    let interpret logger genv senv = function A.Ou -> A.Eh
  end  
  
module TCPCS = ApiComm.MakeBinTCPClientServer A
module Server = TCPCS.Server I
module Client = TCPCS.Client

let logger = new StdLogger.color_console_logger ()
let port = 1123

;;

if Array.length Sys.argv <> 2 then begin printf "usage: %s <server | client>\n%!" Sys.argv.(0); exit 1 end;
let mode = Sys.argv.(1) in

if mode = "server" then 
    let _ = Server.create_passive_server logger "prova" () (logger, "prova", 100, port)
    in
        Unix.pause ()
else if mode = "client" then begin
    let ch = Client.active_connect (logger, "localhost", port)
    in
        ignore (Client.exec logger ch A.Ou);
        Client.disconnect logger ch
  end
