%{
  open MinicAstTypes
  
  exception TypeError

  let struct_defs =
    Hashtbl.create 10

  let typ_of_string s =
    match s with
    | "int" -> Int
    | "bool" -> Bool
    | "void" -> Void
    | _ -> if (Hashtbl.mem struct_defs s) then
        Hashtbl.find struct_defs s
      else
        begin
          Printf.printf
            "Undefined type: %s\n"
            s
          ;
          raise TypeError
        end

  let locals_tmp: (string * typ) list ref = ref []

  let add_local l value =
    let () = locals_tmp := !locals_tmp@[l] in
    match value with
      | None -> []
      | Some n -> [Set(Get(fst l), n)]

  let reset_locals =
    locals_tmp := []

  let (globals_assign: seq ref) = ref []

  let add_globals_assign s e =
    globals_assign := !globals_assign@[Set(Get s, e)]

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
%token EQ LTH GTH GEQ LEQ EQEQ NEQ PLUS TIMES BY AND OR MOD ARROW
%token ADDRESS
%token MINUS NOT
%token TRUE FALSE
%token PUTCHAR
%token EOF
%token STRUCT

%left OR
%left AND
%left NEQ EQEQ
%left LTH GTH GEQ LEQ
%left PLUS MINUS
%left TIMES BY MOD
%right NOT ADDRESS
%left ARROW DOT

%start prog
%type <MinicAstTypes.prog> prog
%type <MinicAstTypes.expr> expr
%type <MinicAstTypes.expr> access

%%

prog:
    struct_def prog {
      {
        structs = $1::$2.structs;
        globals = $2.globals;
        functions = $2.functions;
      }
    }
    | suite_prog {
      let assign_func =
        {
          name = "globals_assign";
          params = [];
          return = Void;
          locals = [];
          code = !globals_assign;
        }
        in
        {
          structs = [];
          globals = $1.globals;
          functions = assign_func::$1.functions
        }
      }
;

suite_prog:
  glob_var SEMI suite_prog {
      {
        structs = [];
        globals = $1::$3.globals;
        functions = $3.functions;
      }
    }
  | funcs EOF {
    {
      structs = [];
      globals= [];
      functions = $1;
    }
  }
;

struct_def:
  id = struct_prefix d = struct_body SEMI { Hashtbl.add struct_defs id (Struct(id, d)); (id, d) }
;

funcs:
  func { [$1] }
  | funcs func { $1@[$2] }
;

struct_body:
  BRAO d = nonempty_list(decl SEMI {$1}) BRAC { d }
;

decl:
  t = TYPE id = IDENT { (id, typ_of_string t) }
  | s = struct_prefix id = IDENT { (id, typ_of_string s) }
  | pointer_decl { $1 }
;

struct_prefix:
  STRUCT id=IDENT { id }
;

pointer_decl:
  s = struct_prefix nonempty_list(TIMES {incr pointer_num}) id = IDENT {
      let typ = build_ptr (!pointer_num) (typ_of_string s) in
      pointer_num := 0;
      (id, typ)
    }
  | t = TYPE nonempty_list(TIMES {incr pointer_num}) id = IDENT {
      let typ = build_ptr (!pointer_num) (typ_of_string t) in
      pointer_num := 0;
      (id, typ)
    }
;

glob_var:
  d = decl e = option(EQ expr { $2 }) {
    let id, _ = d in
    match e with
      | None -> d
      | Some v ->
        add_globals_assign id v;
        d
  }
;


func:
  decl PARO p = params_opt PARC BRAO b = body BRAC {
      let (n, t) = $1 in
      let body = if n = "main" then
          Expr((Call ("globals_assign", [])))::b
        else
          b
      in
      let func_def = {
        name = n;
        params = p;
        return = (t);
        locals = !locals_tmp;
        code = body;
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
;

local:
  d = decl e = option(EQ expr {$2}) SEMI { add_local d e }
;

putchar:
  PUTCHAR PARO expr PARC SEMI { Putchar ($3) }
;

set:
  access EQ expr SEMI { Set ($1, $3) }
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

init_list:
  BRAO separated_list(COMMA, expr) BRAC { InitList($2) }
;

expr:
  CONST { Cst($1) }
  | add { $1 }
  | sub { $1 }
  | mul { $1 }
  | div { $1 }
  | mod_op { $1 }
  | lth { $1 }
  | gth { $1 }
  | leq { $1 }
  | geq { $1 }
  | eq  { $1 }
  | neq { $1 }
  | and_op { $1 }
  | or_op { $1 }
  | call { $1 }
  | bool { $1 }
  | not { $1 }
  | neg { $1 }
  | access { $1 }
  | address { $1 }
  | init_list { $1 }
  | PARO expr PARC { $2 }
;

access:
  get { $1 }
 | deref {$1}
 | struct_access { $1 }
 | ptr_member_access { $1 }
;

add:
  expr PLUS expr { BinOp(Plus, $1, $3) }
;

sub:
  expr MINUS expr { BinOp(Minus, $1, $3) }
;

mul:
  expr TIMES expr { BinOp(Times, $1, $3) }
;

div:
  expr BY expr { BinOp(By, $1, $3) }
;

mod_op:
  expr MOD expr { BinOp(Mod, $1, $3) }
;

lth:
  expr LTH expr { BinOp(Lth, $1, $3) }
;

gth:
  expr GTH expr { BinOp(Gth, $1, $3) }
;

leq:
  expr LEQ expr { BinOp(Leq, $1, $3) }
;

geq:
  expr GEQ expr { BinOp(Geq, $1, $3) }
;

eq:
  expr EQEQ expr { BinOp(Eq, $1, $3) }
;

neq:
  expr NEQ expr { BinOp(Neq, $1, $3) }
;

and_op :
  expr AND expr { BinOp(And, $1, $3) }
;

or_op :
  expr OR expr { BinOp(Or, $1, $3) }
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
  ADDRESS expr { Address($2) }
;

struct_access:
  name = expr DOT member = IDENT { StructMember(name, member) }
;

ptr_member_access:
  name = expr ARROW member = IDENT { StructPtrMember(name, member) }
;

args_opt:
    { [] }
  | args { $1 }
;

args:
    expr { [$1] }
  | expr COMMA args { $3@[$1] }
;
