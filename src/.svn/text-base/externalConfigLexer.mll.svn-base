(*
 * Common
 * externalConfigLexer.mll: lexer definition
 *
 * (C) 2004 Quendi srl.
 *)

{

open Prelude
open ExternalConfigParser
open Printf

let line_count = ref 1
let next_line () = incr line_count

let ascii_char s = Char.chr (int_of_string (String.sub s 1 3))

let clear_lexer () = line_count := 1

}

let backslash 	= "\\\\"
let eol 		= "\\n"
let cr 			= "\\r"
let backspace 	= "\\b"
let tab 		= "\\t"
let ascii		= '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
let num 		= ['0'-'9']+
let id			= ['a'-'z' 'A'-'Z' '_' '0'-'9']

rule string_literal buf = parse
    '"'             { Buffer.contents buf }
  | "\\\""	        { Buffer.add_char buf '"'; string_literal buf lexbuf }
  | backslash		{ Buffer.add_char buf '\\'; string_literal buf lexbuf }
  | eol				{ Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | cr				{ Buffer.add_char buf '\r'; string_literal buf lexbuf }
  | backspace		{ Buffer.add_char buf '\b'; string_literal buf lexbuf }
  | tab				{ Buffer.add_char buf '\t'; string_literal buf lexbuf }
  | ascii			{ Buffer.add_char buf (ascii_char (Lexing.lexeme lexbuf));
  					  string_literal buf lexbuf }
  | _				{ let c = Lexing.lexeme_char lexbuf 0 in
  					  Buffer.add_char buf c;
  					  string_literal buf lexbuf }

and path_literal buf = parse
    '>'             { Buffer.contents buf }
  | _				{ let c = Lexing.lexeme_char lexbuf 0 in
  					  Buffer.add_char buf c;
  					  path_literal buf lexbuf }
  					  
and comment = parse
 	'\n'
  | eof
  | "\r\n"						{ () }
  | _							{ ignore (Lexing.lexeme lexbuf); comment lexbuf }


and token = parse
    eof             			{ EOF }
  | [' ' '\t']+      			{ token lexbuf }
  | '\n'						
  | "\r\n"						{ next_line (); token lexbuf }
  | "true"						{ BOOL true }
  | "false"						{ BOOL false }
  | "enum"						{ ENUM }
  | '='							{ EQ }
  | ';'							{ SEMICOLON }
  | '|'							{ PIPE }
  | ','							{ COMMA }
  | '('							{ BRA }
  | ')'							{ KET }
  | '['							{ SQBRA }
  | ']'							{ SQKET }
  | '{'							{ CBRA }
  | '}'							{ CKET }
  | '<'							{ PATH (path_literal (Buffer.create 10) lexbuf) }
  | '"'             			{ STRING (string_literal (Buffer.create 10) lexbuf) }
  | '#'							{ comment lexbuf; next_line (); token lexbuf }
    
  | '\'' backslash '\''			{ CHAR '\\' }
  | '\'' eol '\''				{ CHAR '\n' }
  | '\'' cr '\''				{ CHAR '\r' }
  | '\'' backspace '\''			{ CHAR '\b' }
  | '\'' tab '\''				{ CHAR '\t' }
  | '\'' ascii '\''				{ CHAR (ascii_char (String.sub (Lexing.lexeme lexbuf) 1 4)) }
  | '\'' _ '\''					{ CHAR ((String.sub (Lexing.lexeme lexbuf) 1 2).[0]) }
  | "\\'"						{ CHAR '\'' }
  
  
  | '-'? num? '.' num?			{ FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '-'? num					{ INT (int_of_string (Lexing.lexeme lexbuf)) }
    
  | ['a'-'z' 'A'-'Z' '_'] (id)*
  | ['0'-'9'] (id)+				{ ID (Lexing.lexeme lexbuf) }
  
  | '\'' (id)+					{ let s = Lexing.lexeme lexbuf in ENUMID (String.sub s 1 (String.length s - 1)) }	
  								
  | _							{ raise (Failure (sprintf "illegal token %S" (Lexing.lexeme lexbuf))) }

