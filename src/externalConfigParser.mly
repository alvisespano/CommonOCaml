/*
 * Lordaeron
 * Lichking client daemon
 * syslogParser.mly: parser definition
 *
 * (C) 2004 Quendi srl.
 */

%{
open ExternalConfigAbsyn
%}

%token EOF EQ BRA KET SQBRA SQKET CBRA CKET SEMICOLON COMMA SEMICOLON PIPE ENUM
%token <string> STRING ID PATH ENUMID
%token <char> CHAR
%token <int> INT
%token <bool> BOOL
%token <float> FLOAT

%start file
%type <ExternalConfigAbsyn.file> file
%%

file:
	record EOF						{ ([], $1) }
  | enums record EOF				{ ($1, $2) }
	
enums:
	enum							{ [$1] }
  | enum enums						{ $1 :: $2 }

enum:
	ENUM id EQ enum_members			{ ($2, $4) }
	
enum_members:
	id								{ [$1] }
  | id PIPE enum_members			{ $1 :: $3 }

record:
	binding							{ [$1] }
  | binding SEMICOLON				{ [$1] }
  | binding record					{ $1 :: $2 }
  | binding SEMICOLON record		{ $1 :: $3 }

binding:
  	id EQ value			{ ($1, $3) }

id:
	ID					{ $1 } 	
  	
value:
	FLOAT				{ Float $1 }
  | INT					{ Int $1 }
  | BOOL				{ Bool $1 }
  | STRING				{ String $1 }
  | CHAR				{ Char $1 }
  | SQBRA list SQKET	{ List $2 }
  | SQBRA SQKET         { List [] }
  | BRA tuple KET		{ Tuple $2 }
  | CBRA record CKET	{ Record $2 }
  | PATH				{ File $1 }
  | ENUMID				{ EnumId $1 }
    
list:
	value					{ [$1] }
  | value SEMICOLON			{ [$1] }
  | value list				{ $1 :: $2 }
  | value SEMICOLON list	{ $1 :: $3 }
  
tuple:
	value COMMA value		{ [$1; $3] }
  | value COMMA tuple		{ $1 :: $3 }
  
%%

