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
  | "VAR" { VAR } | "SET" { SET }
  | "PROC" { PROC } | "CALL" { CALL }
  | "IF" { IFS } | "WHILE" { WHILE }
  | "int" { INT } | "bool" { BOOL } | "void" { VOID } | "vec" { VEC }
  | "true" { TRUE } | "false" { FALSE }
  | "not" { NOT } | "and" { AND } | "or" { OR }
  | "eq" { EQ } | "lt" { LT }
  | "add" { ADD } | "sub" { SUB } | "mul" { MUL } | "div" { DIV }
  | "if" { IF }
  | "len" { LEN } | "nth" { NTH } | "alloc" { ALLOC }
  | ('-'?)['0'-'9']+ { NUM (int_of_string(Lexing.lexeme lexbuf)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
