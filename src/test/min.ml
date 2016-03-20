

let l = [-4; 5; 7; 0; -3; 5; 0; -4; 8]

let main () = 
	let mi = XList.min l in
	let ma = XList.max l 
	in
		Printf.printf "min: %d; max: %d\n" mi ma
	
;;

main ()

