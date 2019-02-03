{
open Parser
exception Eof
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | '[' { LBRACKET } | ']' { RBRACKET }
  | '(' { LPAR } | ')' { RPAR }
  | ';' { SEMICOL } | ':' { PPOINT } | ',' { COMMA } | '*' { STAR } | "->" { ARROW }
  | "CONST" { CONST } | "FUN" { FUN } | "REC" { REC }
  | "ECHO" { ECHO }
  | "int" { INT } | "bool" { BOOL }
  | "true" { TRUE } | "false" { FALSE }
  | "not" { NOT } | "and" { AND } | "or" { OR }
  | "eq" { EQ } | "lt" { LT }
  | "add" { ADD } | "sub" { SUB } | "mul" { MUL } | "div" { DIV }
  | "if" { IF }
  | ('-'?)['0'-'9']+ { NUM (int_of_string(Lexing.lexeme lexbuf)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
