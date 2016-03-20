
open Printf


let end_token = "#END#"

let main s = printf "%s ends with %s: %b\n" s end_token (XString.ends_with end_token s)

;;

main Sys.argv.(1)


