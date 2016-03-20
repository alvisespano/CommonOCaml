(*
 * Common library
 * xStream.ml: extended streams
 *
 * (C) 2005 Quendi srl.
 *)
 
(** Stdlib Stream module extension. *) 
 
open Prelude
open XString 
open Printf

(** Converts a stream into a list. *)
let list_of_stream str =
	let rec p l = parser
		[< 'x; str >] -> p (x :: l) str
	  | [< >] 		  -> l
	in
		List.rev (p [] str)

(** Converts a list into a stream. *)		
let stream_of_list = Stream.of_list	

(** Polymorphic identity function for streams. *)
let stream_ident x = [< 'x >]	

(** Polymoprhic identity function for parsers. *)
let parse_ident = parser [< 'x >] -> x

(** Streams a value of type [option] given a streamer for the member data. *)
let stream_option streamer = function
	None   -> [< '"none" >]
  | Some x -> [< '"some"; streamer x >]
  
(** Parses a value of type [option] given a parser for the member data. *)
let parse_option parse = parser
	[< '"none" >] 			 -> None
  | [< '"some"; x = parse >] -> Some x
	
(** Streams a list given a streamer for the element type. *)
let stream_list streamer l =
	let rec f = function
       	[]		-> [< '"]" >]
	  | x :: xs	-> [< streamer x; f xs >]
	in
		[< '"["; f l >]
		
(** Parses a list given a parser for the element type. *)
let parse_list parse =
	let rec p l = parser
		[< '"]"; str >] 	 -> l
      | [< x = parse; str >] -> p (x :: l) str
	in
		parser [< '"["; l = p [] >] -> List.rev l

(** Streams a list given a streamer for the element type simply as a series of tokens with no syntax. *)
let stream_all_list streamer l =
	let rec f = function
       	[]		-> [< >]
	  | x :: xs	-> [< streamer x; f xs >]
	in
		[< f l >]
		
(** Parses a list given a parser for the element type simply as a series of tokens with no syntax. *)
let parse_all_list parse =
	let rec p l = parser
        [< x = parse; str >] -> p (x :: l) str
   	  |	[< >]            	 -> l
    in
		parser [< l = p [] >] -> List.rev l

(** Tokenizes the string [s] using [sep] as separator and [quote] as quotation character. The quoter makes the tokenizer skip separators
until the next occurence of the quoter.
*)
let tokenize ~sep ~quote s =
	let buff = Buffer.create 100 in
	let add c = Buffer.add_char buff c in
	let flush () =
		let r = Buffer.contents buff
		in
			Buffer.clear buff;
			unescape [quote] r
	in
	let rec pquote = parser
		[< ''\\'; 'c; str >] ->
	  		if not (c = quote || c = '\\') then add '\\';
	  		add c;
	  		pquote str
	  	  		
	  | [< 'c; str >] -> if c = quote then () else (add c; pquote str)
	  
	  | [< >] -> raise (Failure "unterminated quote")
	in
	let rec psep = parser
		[< ''\\'; 'c; _ >] ->
	  		if not (c = quote || c = '\\') then add '\\';
	  		add c
	  		
	  |	[< 'c; str >] ->
			if c = sep then psep str
			else if c = quote then pquote str
			else add c
			
	  |	[< >] -> ()
	in
	let rec p l = parser
		[< ''\\'; 'c; str >] ->
	  		if not (c = quote || c = '\\') then add '\\';
	  		add c;
	  		p l str
	  		
  	  | [< 'c; str >] ->
  	  		if c = quote then (pquote str; p l str)
  	  		else if c = sep then let tk = flush () in (psep str; p (tk :: l) str)
  	  		else (add c; p l str)
  	  		
  	  | [< >] -> flush () :: l
	in
		stream_of_list (List.rev (p [] (Stream.of_string (trim sep s))))

(** Renders a string given a stream of tokens, a separator and a quoter. *)
let concat_stream ~sep ~quote str =
	let f s =
		if String.length s = 0 then sprintf "%c%c" quote quote
		else
			let s = escape [quote] s
			in
				if String.contains s sep then sprintf "%c%s%c" quote s quote
				else s
  	in
		mappen_strings f ~sep:(string_of_char sep) (list_of_stream str)
        
(** Pretty prints a stream. *)        
let pretty_stream ?(sep = ' ') ?(quote = '\"') str = concat_stream ~sep ~quote str

(** Streams a string to a string stream. *) 
let stream_string = stream_ident

(** Streams an int to a string stream. *) 
let stream_int n = [< '(string_of_int n) >]

(** Streams a bool to a string stream. *) 
let stream_bool b = [< '(string_of_bool b) >]

(** Streams a float to a string stream. *) 
let stream_float x = [< '(string_of_float x) >]

(** Parses a string from a string stream. *) 
let parse_string = parse_ident

(** Parses an int from a string stream. *) 
let parse_int = parser [< 'n >] -> int_of_string n

(** Parses a bool from a string stream. *) 
let parse_bool = parser [< 'b >] -> bool_of_string b

(** Parses a float from a string stream. *) 
let parse_float = parser [< 'x >] -> float_of_string x


(** Raised by the label parsers when a label is not found. *)
exception Label_not_found of string

(** Parses stream [str] for token pairs when the first must be a label occurring in [lbs] and the second a streamed value associated to that value.
This makes possibile the parsing of record-like structures.
*)
let parse_labels lbs str =
    let rec p env = parser
        [< 'lb; 'x; str >]  -> if XList.occurs lb lbs then p (Env.bind env lb x) str else p env str
      | [< >]               -> env
    in
    let env = p Env.empty str
    in
      object (self)
        (** Looks up label [lb] and applies [f] to its value. Raises [Label_not_found] if not found. *)
        method lookup : 'a. (string -> 'a) -> string -> 'a = fun f lb ->
            match self#lookup_optional f lb with
                Some x -> x
              | None   -> raise (Label_not_found lb)
              
        (** Attempts to lookup label [lb] and applies [f] to its value. *)
        method lookup_optional : 'a. (string -> 'a) -> string -> 'a option = fun f lb ->
            match Env.lookup env lb with
                Some x -> Some (f x)
              | None   -> None
              
        (** Looks up label [lb], tokenizes its associated value using [sep] as separator and [quote] as quoter and subparses the resulting tokens
        with parser [p]. Raises [Label_not_found] if not found.
        *)
        method lookup_sub : 'a. sep:char -> quote:char -> (string Stream.t -> 'a) -> string -> 'a = 
			fun ~sep ~quote p lb ->
	            let s = self#lookup ident lb in
	            let str = tokenize ~sep ~quote s
	            in
	                p str

		method lookup_optional_sub: 'a. sep:char -> quote:char -> (string Stream.t -> 'a) -> string -> 'a option = 
			fun ~sep ~quote p lb -> 
				try Some (self#lookup_sub ~sep ~quote p lb)	
				with Label_not_found _ -> None
        
        (** Looks up label [lb], tokenizes its associated value using [sep] as separator and [quote] as quoter and subparses the resulting tokens
        as a list with parser [p] for each element. Raises [Label_not_found] if not found.
        *)        
        method lookup_sublist : 'a. sep:char -> quote:char -> (string Stream.t -> 'a) -> string -> 'a list = fun ~sep ~quote p lb ->
            self#lookup_sub ~sep ~quote (fun str -> List.map (fun s -> p (tokenize ~sep ~quote s)) (list_of_stream str)) lb
      end

(** Streams a string pair label-value where the label is [lb] and the value [f x]. *)
let stream_label lb f x = [< 'lb; '(f x) >]

(** Streams a string pair label-value only, where the label is [lb] and the value [f x], only if [x] is something. *)
let stream_optional_label lb f = function None -> [< >] | Some x -> stream_label lb f x

(** Streams a string pair label-value where the label is [lb] and the value is the stream render of [str x] given separator [sep] and quoter [quote]. *)
let stream_label_sub ~sep ~quote lb str x = [< 'lb; '(concat_stream ~sep ~quote (str x)) >]

(** Streams a string pair label-value where the label is [lb] and the value is the stream render of a list whare each element is streamed by [str].
[sep] is the separator and [quote] the quoter.
*)
let stream_label_sublist ~sep ~quote lb str xs = stream_label_sub ~sep ~quote lb stream_of_list (List.map (fun x -> concat_stream ~sep ~quote (str x)) xs)

(** Streams an optional pair label-value where label is [lb] and the value is the stream render of [str x] given separator [sep] and quoter [quote]. *)
let stream_optional_label_sub ~sep ~quote lb str = function
	None   -> [< >]
  | Some x -> stream_label_sub ~sep ~quote lb str x 

