%{
  open Ast_types

  let struct_defs =
    Hashtbl.create 10

  let typ_of_string s =
    match s with
    | "int" -> Int
    | "bool" -> Bool
    | "void" -> Void
    | _ -> if (Hashtbl.mem struct_defs s) then
        let ( name, members ) = Hashtbl.find struct_defs s in
        Struct(name, members) 
      else
        failwith (Printf.sprintf "Unknown type: %s" s)

  let locals_tmp: (string * typ) list ref = ref []

  let add_local l value =
    let _ = locals_tmp := !locals_tmp@[l] in
    match value with
      | None -> []
      | Some n -> [Set(fst l, n)]

  let add_local_struct ident members_val =
    let _ = locals_tmp := !locals_tmp@[ident] in
    match members_val with
     | None -> []
     | Some v -> [SetStruct((fst ident), v)]

  let reset_locals =
    locals_tmp := []

  let (globals_assign: seq ref) = ref []

  let add_globals_assign s e =
    globals_assign := !globals_assign@[Set(s, e)]

  let pointer_num = ref 0

  let rec build_ptr num typ =
    if num = 1 then
      Ptr(typ)
    else
      if num < 1 then
        raise (Invalid_argument "Num under one.")
      else
        build_ptr (num-1) (Ptr(typ))
%}

%token <string> TYPE IDENT
%token <int> CONST
%token PARO PARC COMMA SEMI BRAO BRAC DOT
%token IF ELSE WHILE RETURN
%token EQ LTH GTH GEQ LEQ EQEQ NEQ PLUS TIMES BY AND
%token MINUS NOT
%token TRUE FALSE
%token PUTCHAR
%token EOF
%token STRUCT

%right NOT AND
%left NEQ EQEQ
%left LTH GTH GEQ LEQ
%left PLUS MINUS
%left TIMES BY

%start prog
%type <Ast_types.prog> prog

%%

prog:
    structs glob_vars funcs EOF { {structs = $1; globals = $2; functions = $3} }
;

structs:
  list(struct_def) { $1 }
;

struct_def:
  STRUCT id = IDENT BRAO d = struct_type_decls BRAC SEMI { Hashtbl.add struct_defs id (id, d); (id, d) }
;

decl:
    t = TYPE id = IDENT { (id, (typ_of_string t)) }
  | pointer_decl        { $1 }
;

pointer_decl:
  t = TYPE nonempty_list(TIMES {incr pointer_num}) id = IDENT {
    let typ = build_ptr (!pointer_num) (typ_of_string t) in
    pointer_num := 0;
    (id, typ)
  }
;

struct_type_decls:
  nonempty_list(struct_type_decl SEMI {$1}) { $1 }
;

struct_type_decl:
    d = decl { d }
  | s = struct_decl { s }
;

glob_vars:
  | glob_var { [$1] } 
  | glob_vars glob_var { $1@[$2] }
;

glob_var:
    d = decl cst = opt_const SEMI { 
      let _ = match cst with
            | Some(n) -> (add_globals_assign (fst d) n)
            | None -> ()
      in
      d
    }
  | struct_decl opt_init_list SEMI { $1 }
;


struct_decl:
  n = IDENT id = IDENT { (id, (typ_of_string n)) }
;

opt_init_list:
  option(EQ BRAO l = separated_list(COMMA, expr) BRAC { l }) { $1 }
;

funcs:
  func { [$1] }
  | funcs func { $1@[$2] }
;

opt_const:
    { None }
  | EQ CONST { Some (Cst $2) }
;

func:
  struct_type_decl PARO p = params_opt PARC BRAO b = body BRAC {
      let (n, t) = $1 in
      let func_def = {
        name = n;
        params = p;
        return = (t);
        locals = !locals_tmp;
        code = b;
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
  | l = local { l }
  | body instr { $1@[$2] }
  | body l = local { $1@l }
;

instr:
   putchar { $1 }
 | set { $1 }
 | ifi { $1 }
 | whilei { $1 }
 | returni { $1 }
 | expr SEMI { Expr ($1) }
 | set_struct { $1 }
 | set_struct_member { $1 }
 | set_ptr_val { $1 }
;

local:
    d = decl e = opt_const SEMI { add_local d e }
  | s = struct_decl il = opt_init_list SEMI { add_local_struct s il }
;

putchar:
  PUTCHAR PARO expr PARC SEMI { Putchar ($3) }
;

set:
  IDENT EQ expr SEMI { Set ($1, $3) }
;

ifi:
  IF PARO cond = expr PARC BRAO th = body BRAC ELSE BRAO el = body BRAC {
    If (cond, th, el)
  }
;

whilei:
  WHILE PARO cond = expr PARC BRAO b = body BRAC {
    While (cond, b)
  }
;

returni:
  RETURN expr SEMI { Return ($2) }
;

set_struct:
  id = IDENT EQ members = init_list SEMI { SetStruct(id, members) }
;

set_struct_member:
  name_member = struct_access EQ e = expr SEMI { 
  let (name, member) = name_member in
  SetStructMember(name, member, e) }
;

set_ptr_val:
  d = deref EQ e = expr SEMI { SetPtrVal(d, e) }
;

init_list:
  BRAO separated_list(COMMA, expr) BRAC { $2 }
;

expr:
  CONST { Cst($1) }
  | add { $1 }
  | sub { $1 }
  | mul { $1 }
  | div { $1 }
  | lth { $1 }
  | gth { $1 }
  | leq { $1 }
  | geq { $1 }
  | eq  { $1 }
  | neq { $1 }
  | get { $1 }
  | call { $1 }
  | bool { $1 }
  | not { $1 }
  | neg { $1 }
  | deref { $1 }
  | address { $1 }
  | name_member = struct_access {
      let (name, member) = name_member in
      StructMember(name, member)
    }
  | PARO expr PARC { $2 }
;

add:
  expr PLUS expr { Add($1, $3) }
;

sub:
  expr MINUS expr { Sub($1, $3) }
;

mul:
  expr TIMES expr { Mul($1, $3) }
;

div:
  expr BY expr { Div($1, $3) }

lth:
  expr LTH expr { Lth($1, $3) }
;

gth:
  expr GTH expr { Gth($1, $3) }
;

leq:
  expr LEQ expr { Leq($1, $3) }
;

geq:
  expr GEQ expr { Geq($1, $3) }
;

eq:
  expr EQEQ expr { Eq($1, $3) }
;

neq:
  expr NEQ expr { Neq($1, $1) }
;

get:
  IDENT { Get ($1) }
;

call:
  IDENT PARO args_opt PARC { Call ($1, $3) }
;

not:
  NOT expr { Not($2) }
;

neg:
  MINUS expr { Neg($2) }
;

bool:
    TRUE { BoolLit(true) }
  | FALSE { BoolLit(false) }
;

deref:
  TIMES expr { Deref($2) }
;

address:
  AND expr { Address($2) }
;

struct_access:
  name = IDENT DOT member = IDENT { (name, member) }
;

args_opt:
    { [] }
  | args { $1 }
;

args:
    expr { [$1] }
  | expr COMMA args { $3@[$1] }
;
