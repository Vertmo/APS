%{
    open Ast
%}

%token LBRACKET RBRACKET
%token LPAR RPAR
%token SEMICOL PPOINT
%token COMMA
%token STAR
%token ARROW
%token CONST FUN REC
%token ECHO
%token INT BOOL
%token TRUE FALSE
%token NOT AND OR
%token EQ LT
%token ADD SUB MUL DIV
%token IF
%token <int> NUM
%token <string> IDENT
%token EOF

%start<Ast.prog> prog;;
%type<Ast.prog> cmds;;
%type<Ast.cmd> stat;;
%type<Ast.cmd> dec;;
%type<Ast.typeE> typeE;;
%type<Ast.arg> arg;;
%type<Ast.arg list> args;;
%type<Ast.typeE list> typeEs;;
%type<Ast.opPrim> opPrim;;
%type<Ast.expr> expr;;
%type<Ast.expr list> exprs;;
%%

prog:
  | LBRACKET cmds RBRACKET EOF { $2 }
;;

cmds:
  | stat { [$1] }
  | dec SEMICOL cmds { $1::$3 }
  | stat SEMICOL cmds { $1::$3 }
;;

stat: ECHO expr { Echo $2 };;

dec:
  | CONST IDENT typeE expr { ConstDec ($2, $3, $4) }
  | FUN IDENT typeE LBRACKET args RBRACKET expr { FunDec ($2, $3, $5, $7) }
  | FUN REC IDENT typeE LBRACKET args RBRACKET expr { RecFunDec ($3, $4, $6, $8) }
;;

typeE:
  | INT { Int }
  | BOOL { Bool }
  | LPAR typeEs ARROW typeE RPAR { Fun ($2, $4) }
;;

typeEs:
  | typeE { [$1] }
  | typeE STAR typeEs { $1::$3 }
;;

arg: IDENT PPOINT typeE { ($1, $3) };;

args:
  | arg { [$1] }
  | arg COMMA args { $1::$3 }
;;

opPrim:
  | ADD { Add } | SUB { Sub } | MUL { Mul } | DIV { Div } | EQ { Eq } | LT { Lt }
  | AND { And } | OR { Or } | NOT { Not }
;;

expr:
  | TRUE { Bool(true) } | FALSE { Bool(false) }
  | NUM { IntConst($1) }
  | IDENT { Var($1) }
  | LPAR IF expr expr expr RPAR { If($3, $4, $5) }
  | LPAR opPrim exprs RPAR { Op($2, $3) }
  | LBRACKET args RBRACKET expr { Lambda($2, $4) }
  | LPAR expr exprs RPAR { Funcall($2, $3) }
;;

exprs:
  | expr { [$1] }
  | expr exprs { $1::$2 }
;;
