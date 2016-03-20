(*
 * Lordaeron
 * Common daemon library
 * signal.ml: POSIX signals wrapper
 *
 * (C) 2005 Quendi srl.
 *)
 
INCLUDE "switches.ml" 

open Printf

IFDEF THREADED_SIGNALS THEN

let sigint = 2
let sigpipe = 13
let sigalrm = 14
let sigterm = 15

let __signal_handlers =
	let dummy = fun _ -> () in
	new XThread.sync [
		sigint, ref dummy;
	  	sigalrm, ref dummy;
	  	sigterm, ref dummy;
	  	sigpipe, ref dummy;
	  	]

let __signal_dispatcher () =
	let all_signs = __signal_handlers#apply (List.map (fun (n, _) -> n))
	in
		while true do
			let n = Thread.wait_signal all_signs in
			let h = __signal_handlers#apply (fun self -> !(List.assoc n self))
			in
				ignore (XThread.create (sprintf "signalhandler(%d)" n) h n)
		done
		
let on_signal n h =
	__signal_handlers#apply (fun self -> List.assoc n self := h);
	IFNDEF WIN32 THEN ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [n]) ELSE () ENDIF

let _ = ignore (XThread.create "signaldispatcher" __signal_dispatcher ())

ELSE	

let sigint = Sys.sigint
let sigpipe = Sys.sigpipe
let sigalrm = Sys.sigalrm
let sigterm = Sys.sigterm

let on_signal n h =
	Sys.set_signal n (Sys.Signal_handle h);
	IFNDEF WIN32 THEN ignore (Thread.sigmask Unix.SIG_UNBLOCK [n]) ELSE () ENDIF

ENDIF
