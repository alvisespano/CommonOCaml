(*
 * Angband
 * Common daemon library
 * io.ml: I/O facilities
 *
 * (C) 2005 Quendi srl.
 *)
 
(** I/O primitives. *) 
 
open Printf
open Prelude
open XString

	
(** Reads all data available on file descriptor [fd]. *) 
let read_file fd =
	let ic = Unix.in_channel_of_descr fd in
	let len = in_channel_length ic in
	let buf = String.create len
	in
		really_input ic buf 0 len;
		buf

(** Writes [data] on file descriptor [fd]. *)
let write_file fd data =
	let len = String.length data
	in
		ignore (Unix.write fd data 0 len)
		
(** Load file with name [name]. *)		
let load_file name =
	let fd = Unix.openfile name [Unix.O_RDONLY] 0
	in
	    try
	        let data = read_file fd
	        in
		        Unix.close fd;
		        data
		with e -> Unix.close fd; raise e

(** Load a text file with name [name] and split it into lines according to line-terminator [sep] *)
let load_lines ?(sep = "\n") name =
	let s = load_file name
	in
		List.filter (fun s -> String.length s > 0) (Str.split (Str.regexp_string sep) s)		
		
(** Save [data] as file [name] with permissions [mode]. *)		
let save_file name mode data =
	let fd = Unix.openfile name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] mode
	in
	    try
		    write_file fd data;
	    	Unix.close fd
	    with e -> Unix.close fd; raise e

(** Append [data] to file [name], optionally creating it with permission [mode] in case it does not exist. *)
let append_file name mode data =
	let oc = open_out_gen [Open_wronly; Open_append; Open_binary; Open_creat] mode name
	in
		try
		    output_string oc data;
		    flush oc;
		    close_out oc
		with e -> close_out oc; raise e
		
(** Delete file [name]. *)
let delete_file name = Sys.remove name

(** Atomically evaluates function [f] passing the file descriptor of file [name] after having locked it in read/write mode.
[openmode] can be optionally passed as the list of opening options.
*)
let rwlock_file ?(openmode = []) name mode f =
	let fd = Unix.openfile name ([Unix.O_RDWR] @ openmode) mode
	in
		Unix.lockf fd Unix.F_LOCK 0;
		try
			let r = f fd
			in
      			Unix.lockf fd Unix.F_ULOCK 0;
		        Unix.close fd;
		        r
		with e ->
			Unix.lockf fd Unix.F_ULOCK 0;
			Unix.close fd;
			raise e
			
(** Atomically evaluates function [f] passing the file descriptor of file [name] after having locked it in read-only mode.
[openmode] can be optionally passed as the list of opening options.
*)
let rlock_file ?(openmode = []) name mode f =
	let fd = Unix.openfile name ([Unix.O_RDONLY] @ openmode) mode
	in
		Unix.lockf fd Unix.F_RLOCK 0;
		try
			let r = f fd
            in
			    Unix.lockf fd Unix.F_ULOCK 0;
			    Unix.close fd;
                r
		with e ->
			Unix.lockf fd Unix.F_ULOCK 0;
			Unix.close fd;
			raise e
		
(** Gets modification time of file [name]. *)		
let get_modification_time name = (Unix.stat name).Unix.st_mtime


	
(** Type for child processes. *)
type child = int option XThread.sync

(** Creates a dummy child process. *)
let fetus () = new XThread.sync None

(** Kills child [child] with signal [signal]. *)
let kill_child (child : child) signal =
	child#apply (on_some (fun pid -> try Unix.kill pid signal with _ -> ()))

(** Checks whether child [child] is alive. *)
let is_child_alive (child : child) = child#apply is_some

(** Runs shell command [shell] in background, optionally with arguments [argv] and invoking [at_death] at child process death. *)
let run_bg_executable (logger : #Log.logger) ?(argv = [||]) ?(at_death = fun _ -> ()) shell =
	let child = fetus () in
	let shell = Array.of_list (Str.split (Str.regexp "[ ]+") shell)	in
	let f () =
		logger#info (sprintf "executing shell program: %s %s" shell.(0) (flatten_string_array argv));
		let pid = Unix.create_process shell.(0) (Array.append shell argv) Unix.stdin Unix.stdout Unix.stderr in
		child#set (Some pid);
		let (_, pst) = Unix.waitpid [] pid
		in
			(match pst with
				Unix.WEXITED 0   -> logger#info (sprintf "child %d exited cleanly" pid)
			  | Unix.WEXITED 127 -> logger#warn ~code:[1;5;1] (sprintf "shell command %S not found" shell.(0))
			  | Unix.WEXITED n	 -> logger#warn ~code:[1;5;2] (sprintf "child %d exited abnormally with status %d" pid n)
			  | Unix.WSIGNALED n -> logger#warn ~code:[1;5;3] (sprintf "child %d killed by signal %d" pid n)
			  | Unix.WSTOPPED _  -> logger#warn ~code:[1;5;4] (sprintf "child %d stopped" pid)
              );
			child#set None;
			at_death pst
	in		
		ignore (XThread.create logger (sprintf "childlauncher(%s)" shell.(0)) f ());
		child
		
(** Runs shell command [shell] in background, optionally with arguments [argv] and invoking [at_death] at child process death. After [timeout]
seconds, signal [signal] is sent to the child process.
*)
let run_bg_executable_timeout logger ?argv ?at_death ~signal ~timeout shell =
	let child = run_bg_executable logger ?argv ?at_death shell in
	let f () = Thread.delay timeout;
			   if is_child_alive child then kill_child child signal
	in
		child#apply (function
			None	 -> ()
		  |	Some pid ->	ignore (XThread.create logger (sprintf "childkiller(%d:%s)" pid shell) f ()));
		child

(** System syscall wrapper that handles child process exit codes. *)
let system logger s = 
	match Unix.system s with
		Unix.WEXITED n when n = 0	-> ()
	  | Unix.WEXITED n 				-> logger#warn (sprintf "%S exited with code %d" s n)
	  |	Unix.WSIGNALED n			-> logger#warn (sprintf "%S killed by signal %d" s n)
	  | Unix.WSTOPPED n				-> logger#warn (sprintf "%S stopped by signal %d" s n)

(** Respawn child process and make it execute function [f].
[on_exit] is called on child termination.
[on_signal] is called on child kill.
Both functions must return one of the following states:
[`Unlimited_respawn timeout] for forcing a respawn in [timeout] seconds;
[`Limited_continuous_respawn (timeout, n, exit_code)] for forcing a respawn in [timeout] seconds for up to [n] continuous time with the
same exit mode; after [n] times exit with [exit_code];
[`Limited_global_respawn (timeout, n, exit_code)] for forcing a respawn in [timeout] seconds for up to [n] continuous time with any
exit mode; after [n] times exit with [exit_code];
[`Exit] for forcing exit at child death.
*)	  
let respawn logger ~on_exit ~on_signal f =
    let last_exit = ref None and continuous_exits = ref 0 and global_exits = ref 0 in
    let rec recur () =
        let pid = Unix.fork ()
        in
            if pid = 0 then (void (f ()); exit 0)
            else
                let logger = LogLib.prefixer logger ~format:(format_of_string "[%s] %s") (string_of_int (Unix.getpid ())) in
                logger#info (sprintf "waiting for child process %d death" pid);
                let (code, action) =
                    match Unix.waitpid [] pid with
                    (_, Unix.WEXITED n) ->
                        logger#info (sprintf "child %d exited with code %d" pid n);
                        (`Exit n, on_exit logger n)
                        
                  | (_, Unix.WSIGNALED n) ->
                        logger#info (sprintf "child %d has been killed by signal %d" pid n);
                        (`Signal n, on_signal logger n)
                        
                  | (_, Unix.WSTOPPED _) -> raise (Unexpected "respawn: WSTOPPED")
                in
                    match action with
                        `Unlimited_respawn timeout ->
                            logger#info (sprintf "respawning in %g seconds..." timeout);
                            global_exits := 0;
                            continuous_exits := 0;
                            Thread.delay timeout;
                            recur ()
                            
                      | `Limited_continuous_respawn (timeout, n, exit_code) ->
                            let changed =
                                match !last_exit with
                                    None       -> false
                                  | Some code' -> not (code' = code)
                            in
                                last_exit := Some code;
                                if changed then continuous_exits := 0;
                                incr continuous_exits;
                                incr global_exits;
                                if !continuous_exits < n then begin
                                    logger#info (sprintf "child has exited this way for the #%d time. Respawning in %g seconds..." !continuous_exits timeout);
                                    Thread.delay timeout;
                                    recur ()
                                  end
                                else begin
                                    logger#info (sprintf "child has passed the %d-time respawn limit. Exiting with code %d..." n exit_code);
                                    exit exit_code
                                  end
                                                      
                      | `Limited_global_respawn (timeout, n, exit_code) ->
                            if !global_exits < n then begin
                                incr global_exits;
                                logger#info (sprintf "child has exited for the #%d time. Respawning in %g seconds..." !global_exits timeout);
                                Thread.delay timeout;
                                recur ()
                              end
                            else begin
                                logger#info (sprintf "child has passed the %d-time respawn limit. Exiting with code %d..." n exit_code);
                                exit exit_code
                              end
                                                        
                      | `Exit n ->
                            logger#info (sprintf "forcing exit with code %d due to child death..." n);
                            exit n
    in
        recur ()
