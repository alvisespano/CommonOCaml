(*
 * HDA Bridge 
 *
 * (C) 2007 H-Care srl.
 *)

open Printf
open Prelude


let http_server_time () = Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time ())

module type OcamlHttpTransportConfig = sig val server_name : string end

module MakeOcamlHttpTransport (C : OcamlHttpTransportConfig) (A : HttpComm.Api) = 
  struct
	module Api = A
	let receive_http_request _ ((sock, _, _) : CommLib.TCPConnector.channel) =
		let split_path = Str.split (Str.regexp "[/]+") in
		let request = new Http_request.request (Unix.in_channel_of_descr (sock#apply ident)) 
		in 
		  {
			HttpComm.path 	= split_path request#path;
			HttpComm.meth 	= (match request#meth with `GET -> HttpComm.Get | `POST -> HttpComm.Post);
			HttpComm.uri	= request#uri;
			HttpComm.params = 
			  begin
				let tbl = Hashtbl.create 10 
				in
					List.iter (fun (id, value) -> Hashtbl.add tbl id (XString.trim_spaces value)) request#params;
					tbl
			  end;
		  }
	
	let send_http_response logger ((sock, _, _) : CommLib.TCPConnector.channel) response =
		let headers = [
			"Date", http_server_time (); 
			"Server", C.server_name;
			"Content-Type", HttpComm.string_of_content_type A.string_of_custom_content_type response.HttpComm.content_type;
			"Connection", "close"] 
			  @
			(match response.HttpComm.content_length with
				None	-> []
			  | Some n	-> [("Content-Length", string_of_int n)]) 
			  @ 
			response.HttpComm.headers
		in
		let first_line = sprintf "HTTP/1.1 %d %s" response.HttpComm.code response.HttpComm.reason in
		let headers = XString.mappen_strings ~sep:"\r\n" (fun (id, value) -> sprintf "%s: %s" id value) headers in
		let header = sprintf "%s\r\n%s\r\n\r\n" first_line headers
		in
			sock#send_string header; 
			match response.HttpComm.body with 
				`Data data 	-> sock#send_string data
			  | `Stream f 	-> f logger sock
  end

module ParserLib =
  struct 
	let get_string tbl id = 
		try Hashtbl.find tbl id 
		with Not_found -> raise (HttpComm.Parse_error (sprintf "%s not found" id))

	let get_switch tbl id = Hashtbl.mem tbl id

	let get_non_empty_string tbl id = match get_string tbl id with
		""		-> raise (HttpComm.Parse_error (sprintf "%s cannot be empty" id))
	  | s		-> s

	let get_string_option tbl id = 
		try 
			(match (Hashtbl.find tbl id) with "" -> None | s -> Some s)
		with Not_found -> None

	let get_path tbl id = 
		let s = get_string tbl id in
		try ignore (Str.search_forward (Str.regexp "\\.\\.") s 0); 
			raise (HttpComm.Parse_error "path cannot contain \"..\"")
		with Not_found -> s
	
	let get_int tbl id = 
		let s = get_string tbl id
		in
			try int_of_string s
			with Failure _ -> raise (HttpComm.Parse_error (sprintf "%s is not an int: %s" id s))

	let get_int_option tbl id = 
		match get_string_option tbl id with
		  	None	-> None
		  | Some ""	-> None
		  | Some s	-> try Some (int_of_string s) with _ -> raise (HttpComm.Parse_error (sprintf "%s is not an int: %s" id s))

	let get_bool_option tbl id = match get_string_option tbl id with
		None		  
	  | Some ""			-> None
	  | Some "true"	
	  | Some "1"		-> Some true
	  | Some "false"	
	  | Some "0"		-> Some false
	  | Some s			-> raise (HttpComm.Parse_error (sprintf "%s is not a boolean: %s" id s))

	let get_bool tbl id = match get_string tbl id with
		"true"	-> true
	  | "false"	-> false
	  | s		-> raise (HttpComm.Parse_error (sprintf "%s is not a boolean: %s" id s))

  end


module ResponseLib =
  struct
	let __successful ?(content_type = HttpComm.Text) s = {
		HttpComm.code	= 200;
		reason			= "OK";
		content_type	= content_type;
		content_length	= Some (String.length s);
		headers			= [];
		body			= `Data s
	}

	let html = __successful ~content_type:HttpComm.Html
	let xml = __successful ~content_type:HttpComm.Xml  
	let text = __successful ~content_type:HttpComm.Text
	let css = __successful ~content_type:HttpComm.Css
	let favicon data = __successful ~content_type:HttpComm.Ico data

	let unsuccessful http_code s =
		let reason = match http_code with 
			400	-> "Bad request"
		  | 500	-> "Server failure"
		  | 404	-> "Not found" 
		  | n	-> raise (Failure (sprintf "HttpCommLib.unsuccessful: unsupported error code: %d" n))
		in
		let data = sprintf 
					"<html><head><title>%d: %s</title></head><body><h1>%d: %s</h1><p>%s</p></body></html>"
					http_code reason http_code reason s 
		in
		  {
			HttpComm.code			= http_code;
			HttpComm.reason			= reason;
			HttpComm.content_type	= HttpComm.Html;
			HttpComm.content_length	= Some (String.length data);
			HttpComm.headers		= [("X-ErrorMessage", s)];
			HttpComm.body			= `Data data
		  }
  end



