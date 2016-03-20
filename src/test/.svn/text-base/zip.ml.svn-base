

let usage = Printf.sprintf "usage: %s [OPTION]..\n\n OPTION" Sys.argv.(0)

let file = ref ""
let help = ref false
let level = ref 9
let unzip = ref false

let specl =
       ["-level", Arg.Set_int level,  "       set compression level (0-9)";
        "-file", Arg.Set_string file, "        filename";
		"-unzip", Arg.Set unzip,      "       test unzip time";
		]           


let encode level f =
	let input = Io.read_file (Unix.openfile f [Unix.O_RDONLY] 0o400) in
	let starttime = Time.precise_now () in
	let output = Security.crunch ~level input in
	let output_s = match output with Security.Zip s -> s
	in
		let endtime = Time.precise_now () in
		let size_in = String.length input in
		let size_out = String.length output_s in
		let decompress_s = ref "" in
		if !unzip then 
		  begin
			let starttime_d = Time.precise_now () in
			ignore (Security.decrunch output);
			let endtime_d = Time.precise_now () in
			decompress_s := (Printf.sprintf " (unzip: %.2f ms)" ((endtime_d -. starttime_d) *. 1000.0))
		  end;
		Printf.printf "%d -> %d (zip: %.2f ms, level %d)%s\n" size_in size_out  ((endtime -. starttime) *. 1000.0) level !decompress_s
	
;;


Arg.parse specl (fun s -> raise (Arg.Bad s)) usage;

if (!file = "") 
	then Arg.usage specl usage
	else (encode !level !file)

