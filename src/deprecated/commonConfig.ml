(*
 * Lodaeron
 * Common library
 * commonConfig.ml: common configuration
 *
 * (C) 2005 Quendi srl.
 *)

INCLUDE "switches.ml"

open Prelude
 
let getmac_filename = "./getmac.exe"

let default_listen_queue = 50

let default_resident_update_timeout 	    = minutes 1
let default_smart_removable_polling_timeout = secs 3
let default_smart_removable_update_pri		= 10

let default_log_lines_per_chunk = 1000
let default_log_max_chunks = 200

let default_datagram_length = 1024

let default_external_config_file_mode = 0o664

let web_session_key_collector_interval = minutes 20
let default_web_session_expiration = hours 1
