%{
    open Ast
%}

%token LBRACKET RBRACKET
%token LPAR RPAR
%token SEMICOL PPOINT
%token COMMA DOT
%token STAR PLUS
%token ARROW
%token CONST FUN REC
%token ECHO
%token VAR SET PROC CALL IFS WHILE RETURN
%token INT BOOL VOID VEC UNIT PARPAR
%token TRUE FALSE
%token NOT AND OR
%token EQ LT
%token ADD SUB MUL DIV
%token IF
%token LEN NTH ALLOC
%token LET EQUAL IN
%token FST SND
%token INL INR
%token CASE OF DARROW PIPE
%token <int> NUM
%token <string> IDENT
%token <string> TVAR
%token EOF

%start<Ast.prog> main;;
%type<Ast.prog> prog;;
%type<Ast.cmd list> cmds;;
%type<Ast.dec> dec;;
%type<Ast.stat> stat;;
%type<Ast.lval> lval;;
%type<Ast.ret> ret;;
%type<Ast.eType> eType;;
%type<Ast.arg> arg;;
%type<Ast.arg list> args;;
%type<Ast.eType list> eTypes;;
%type<Ast.opPrim> opPrim;;
%type<Ast.expr> expr;;
%type<Ast.expr list> exprs;;
%%

main: prog EOF { $1 };;

prog: LBRACKET cmds RBRACKET { $2 };;

cmds:
  | stat { [Stat $1] }
  | dec SEMICOL cmds { (Dec $1)::$3 }
  | stat SEMICOL cmds { (Stat $1)::$3 }
  | ret { [Ret $1] }
;;

dec:
  | CONST IDENT eType expr { ConstDec ($2, $3, $4) }
  | FUN IDENT eType LBRACKET args RBRACKET expr { FunDec ($2, $3, $5, $7) }
  | FUN REC IDENT eType LBRACKET args RBRACKET expr { RecFunDec ($3, $4, $6, $8) }
  | VAR IDENT eType { VarDec ($2, $3) }
  | PROC IDENT LBRACKET args RBRACKET prog { ProcDec($2, $4, $6) }
  | PROC REC IDENT LBRACKET args RBRACKET prog { RecProcDec($3, $5, $7) }
  | FUN IDENT eType LBRACKET args RBRACKET LBRACKET cmds RBRACKET { FunProcDec($2, $3, $5, $8) }
  | FUN REC IDENT eType LBRACKET args RBRACKET LBRACKET cmds RBRACKET { RecFunProcDec($3, $4, $6, $9) }
;;

stat:
  | ECHO expr { Echo $2 }
  | SET lval expr { Set ($2, $3) }
  | IFS expr prog prog { Ifs($2, $3, $4) }
  | WHILE expr prog { While($2, $3) }
  | CALL IDENT exprs { Call($2, $3) }
;;

ret: RETURN expr { Return $2 }

lval:
  | IDENT { SymLval($1) }
  | LPAR NTH lval expr RPAR { Nth($3, $4) }
;;

eType:
  | INT { Int }
  | BOOL { Bool }
  | VOID { Void } | UNIT { Unit }
  | LPAR VEC eType RPAR { Vec($3) }
  | LPAR eTypes ARROW eType RPAR { Fun ($2, $4) }
  | TVAR { TypeVar($1) }
  | LPAR eType STAR eType RPAR { Product($2, $4) }
  | LPAR eType PLUS eType RPAR { Sum($2, $4) }
;;

eTypes:
  | eType { [$1] }
  | eType STAR eTypes { $1::$3 }
;;

arg: IDENT PPOINT eType { ($1, $3) };;

args:
  | arg { [$1] }
  | arg COMMA args { $1::$3 }
;;

opPrim:
  | ADD { Add } | SUB { Sub } | MUL { Mul } | DIV { Div } | EQ { Eq } | LT { Lt }
  | AND { And } | OR { Or } | NOT { Not }
  | LEN { Len } | NTH { Nth } | ALLOC { Alloc }
;;

expr:
  | TRUE { Bool(true) } | FALSE { Bool(false) }
  | NUM { IntConst($1) }
  | IDENT { Sym($1) }
  | LPAR IF expr expr expr RPAR { If($3, $4, $5) }
  | LPAR opPrim exprs RPAR { Op($2, $3) }
  | LBRACKET args RBRACKET expr { Abs($2, $4) }
  | LPAR expr exprs RPAR { App($2, $3) }
  | LPAR LET IDENT EQUAL expr IN expr RPAR { Let($3, $5, $7) }
  | LPAR expr COMMA expr RPAR { Pair($2, $4) }
  | expr DOT FST { Fst($1) } | expr DOT SND { Snd($1) }
  | LPAR INL eType expr RPAR { InL($3, $4) } | LPAR INR eType expr RPAR { InR($3, $4) }
  | LPAR CASE expr OF INL IDENT DARROW expr PIPE INR IDENT DARROW expr RPAR
      { Case($3, $6, $8, $11, $13) }
  | PARPAR { Unit }
;;

exprs:
  | expr { [$1] }
  | expr exprs { $1::$2 }
;;
