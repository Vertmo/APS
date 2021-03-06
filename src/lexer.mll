{
open Parser
exception Eof
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | '[' { LBRACKET } | ']' { RBRACKET }
  | '(' { LPAR } | ')' { RPAR }
  | ';' { SEMICOL } | ':' { PPOINT } | ',' { COMMA } | '.' { DOT }
  | '*' { STAR } | "->" { ARROW } | "+" { PLUS }
  | "CONST" { CONST } | "FUN" { FUN } | "REC" { REC }
  | "ECHO" { ECHO } | "IGNORE" { IGNORE }
  | "VAR" { VAR } | "SET" { SET }
  | "PROC" { PROC } | "CALL" { CALL }
  | "IF" { IFS } | "WHILE" { WHILE }
  | "int" { INT } | "bool" { BOOL } | "void" { VOID } | "vec" { VEC } | "unit" { UNIT } | "()" { PARPAR }
  | "true" { TRUE } | "false" { FALSE }
  | "not" { NOT } | "and" { AND } | "or" { OR }
  | "eq" { EQ } | "lt" { LT }
  | "add" { ADD } | "sub" { SUB } | "mul" { MUL } | "div" { DIV }
  | "if" { IF }
  | "len" { LEN } | "nth" { NTH } | "alloc" { ALLOC }
  | "RETURN" { RETURN }
  | "let" { LET } | '=' { EQUAL } | "in" { IN }
  | "fst" { FST } | "snd" { SND }
  | "inl" { INL } | "inr" { INR }
  | "case" { CASE } | "of" { OF } | "=>" { DARROW } | '|' { PIPE }
  | "TYPE" { TYPE }
  | ('-'?)['0'-'9']+ { NUM (int_of_string(Lexing.lexeme lexbuf)) }
  | '\''['a'-'z']+ { let s = (Lexing.lexeme lexbuf) in
                     TVAR (String.sub s 1 ((String.length s)-1)) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }
