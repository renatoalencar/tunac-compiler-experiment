{
open Parser
}

let whitespace = [' ' '\t' '\n' '\r']+
let nat = ['0'-'9']+
let int = '-'? nat
let ident = ['a'-'z'] ['a'-'z' '0'-'9']*

rule token = parse
  | whitespace { token lexbuf }
  | '%' (nat as l)   { LOCAL (int_of_string l) }
  | '$' (ident as l) { GLOBAL l }
  | int as l { INT (int_of_string l) }
  | '*' { STAR }
  | ';' { SEMICOLON }
  | ":=" { ASSIGN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "continue" { CONTINUE }
  | "begin" { BEGIN }
  | "end" { END }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "apply" { APPLY }
  | "alloc" { ALLOC }
  | "add" { ADD }
  | "sub" { SUB }
  | eof   { EOF }