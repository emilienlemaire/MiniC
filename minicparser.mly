%{
  open Ast_types

  let typ_of_string s =
    match s with
    | "int" -> Int
    | "bool" -> Bool
    | "void" -> Void
    | _ -> failwith (Printf.sprintf "Unknown type %s" s)

  let locals_tmp: (string * typ) list ref = ref []

  let add_local l =
    locals_tmp := !locals_tmp@[l]

  let reset_locals =
    locals_tmp := []

%}

%token <string> TYPE IDENT
%token <int> CONST
%token PARO PARC COMMA SEMI BRAO BRAC
%token IF ELSE WHILE RETURN
%token EQ LTH PLUS TIMES
%token TRUE FALSE
%token PUTCHAR
%token EOF

%left LTH
%left PLUS
%left TIMES

%start prog
%type <Ast_types.prog> prog

%%

prog:
  glob_vars funcs EOF { {globals = $1; functions = $2} }
;

glob_vars:
  | glob_var { [$1] } 
  | glob_vars glob_var { $1@[$2] }
;

glob_var:
  decl opt_const SEMI { $1 }
;

decl:
  TYPE IDENT { ($2, (typ_of_string $1)) }

funcs:
  func { [$1] }
  | funcs func { $1@[$2] }
;

opt_const:
    {  }
  | EQ CONST {  }
;

func:
  decl PARO params_opt PARC BRAO body BRAC {
      let (n, t) = $1 in
      let func_def = {
        name = n;
        params = $3;
        return = (t);
        locals = !locals_tmp;
        code = $6;
      } in
      reset_locals;
      func_def
  }
;

params_opt:
    { [] }
  | params { $1 }
;

params:
   param { [$1] }
  | param COMMA params { $3@[$1] }
;

param:
  decl { $1 }
;

body:
  instr { [$1] }
  | local { add_local $1; [] }
  | body instr { $1@[$2] }
  | body local { add_local $2; $1 }
;

instr:
   putchar { $1 }
 | set { $1 }
 | ifi { $1 }
 | whilei { $1 }
 | returni { $1 }
 | expr SEMI { Expr ($1) }
;

local:
  decl opt_const SEMI { $1 }
;

putchar:
  PUTCHAR PARO expr PARC SEMI { Putchar ($3) }
;

set:
  IDENT EQ expr SEMI { Set ($1, $3) }
;

ifi:
  IF PARO expr PARC BRAO body BRAC ELSE BRAO body BRAC {
    If ($3, $6, $10)
  }
;

whilei:
  WHILE PARO expr PARC BRAO body BRAC {
    While ($3, $6)
  }
;

returni:
  RETURN expr  SEMI { Return ($2) }

expr:
  CONST { Cst($1) }
  | add { $1 }
  | mul { $1 }
  | lth { $1 }
  | get { $1 }
  | call { $1 }
  | bool { $1 }
  | PARO expr PARC { $2 }

add:
  expr PLUS expr { Add($1, $3) }
;

mul:
  expr TIMES expr { Ast_types.Mul($1, $3) }
;

lth:
  expr LTH expr { Lth($1, $3) }
;

get:
  IDENT { Get ($1) }
;

call:
  IDENT PARO args_opt PARC { Call ($1, $3) }
;

bool:
    TRUE { BoolLit(true) }
  | FALSE { BoolLit(false) }
;

args_opt:
    { [] }
  | args { $1 }
;

args:
    expr { [$1] }
  | expr COMMA args { $3@[$1] }
;
