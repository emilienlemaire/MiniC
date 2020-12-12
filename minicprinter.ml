open Printf
open MinicAstTypes

let num_tabs = ref 0

let rec print_tabs n =
  if n > 0 then
    begin
      printf "  ";
      print_tabs (n-1)
    end
  else
    ()

let rec print_ptr typ acc =
  match typ with
    | Ptr(typ) -> print_ptr typ ("*"^acc)
    | _ -> print_typ typ; printf "%s" acc

and print_typ typ =
  match typ with
    | Int -> printf "int "
    | Bool -> printf "bool "
    | Void -> printf "void "
    | Struct(name, _) -> printf "struct %s " name
    | Ptr(_) -> print_ptr typ ""

let rec print_members members =
  match members with
    | [] -> ()
    | (name, typ)::tl ->
      begin
        print_tabs (!num_tabs);
        print_typ typ;
        printf "%s;" name;
        print_newline ();
        print_members tl
      end

let print_struct s =
  match s with
    | Struct(name, members) ->
      begin
        print_tabs (!num_tabs);
        printf "struct %s {\n" name;
        incr num_tabs;
        print_members members;
        decr num_tabs;
        print_tabs (!num_tabs);
        printf "};\n";
      end
    | _ -> raise (Invalid_argument "Unreachable")

let rec print_structs structs =
  match structs with
  | [] -> ()
  | (name, members)::tl ->
    print_struct (Struct(name, members));
    print_newline ();
    print_structs tl

let print_var var =
  let (name, typ) = var in
  print_tabs (!num_tabs);
  print_typ typ;
  printf "%s;\n" name

let rec print_vars vars =
  match vars with
    | [] -> ()
    | hd::tl ->
      print_var hd;
      print_newline ();
      print_vars tl

let rec print_params params =
  match params with
    | [] -> ()
    | (name, typ)::[] ->
      print_typ typ;
      printf "%s" name
    | (name, typ)::tl ->
      print_typ typ;
      printf "%s, " name;
      print_params tl

let print_binop binop =
  match binop with
    | Plus -> printf "+"
    | Minus -> printf "-"
    | Times -> printf "*"
    | By -> printf "/"
    | Mod -> printf "%%"
    | Lth -> printf "<"
    | Gth -> printf ">"
    | Leq -> printf "<="
    | Geq -> printf ">="
    | Eq -> printf "=="
    | Neq -> printf "!="
    | And -> printf "&&"
    | Or -> printf "||"

let rec print_expr expr =
  match expr with
    | Cst n -> print_int n
    | BinOp (binop, e1, e2) ->
      printf "(";
      print_expr e1;
      printf " ";
      print_binop binop;
      printf " ";
      print_expr e2;
      printf ")"
    | Get n -> printf "%s" n
    | Not e ->
      printf "!(";
      print_expr e;
      printf ")"
    | Neg e ->
      printf "-(";
      print_expr e;
      printf ")";
    | Call (name, args) ->
      printf "%s(" name;
      print_expr_list args;
      printf ")"
    | BoolLit value -> printf "%B" value;
    | StructMember (access, member) ->
      printf "(";
      print_expr access;
      printf ".%s)" member
    | StructPtrMember (access, member) ->
      printf "(";
      print_expr access;
      printf "->%s)" member
    | Deref ptr ->
      printf "(*";
      print_expr ptr;
      printf ")"
    | Address expr ->
      printf "(&";
      print_expr expr;
      printf ")";
    | InitList e ->
      printf "{";
      print_expr_list e;
      printf "}"
and print_expr_list exprs =
  match exprs with
    | [] -> ()
    | hd::[] -> print_expr hd
    | hd::tl ->
      print_expr hd;
      printf ", ";
      print_expr_list tl

let rec print_instr instr =
  match instr with
    | Putchar(e) ->
      printf "putchar(";
      print_expr e;
      printf ");\n"
    | Set(var, value) ->
      print_expr var;
      printf " = ";
      print_expr value;
      printf ";\n"
    | If (cond, th, el) ->
      printf "if (";
      print_expr cond;
      printf ") {\n";
      incr num_tabs;
      print_seq th;
      decr num_tabs;
      print_tabs (!num_tabs);
      printf "} else {\n";
      incr num_tabs;
      print_seq el;
      decr num_tabs;
      print_tabs (!num_tabs);
      printf "}\n";
    | While (cond, seq) ->
      printf "while (";
      print_expr cond;
      printf ") {\n";
      incr num_tabs;
      print_seq seq;
      decr num_tabs;
      print_tabs(!num_tabs);
      printf "}\n";
    | Return e ->
      printf "return ";
      print_expr e;
      printf ";\n"
    | Expr e ->
      print_expr e;
      printf ";\n"
and print_seq seq =
  match seq with
    | [] -> ()
    | hd::tl ->
      print_tabs (!num_tabs);
      print_instr hd;
      print_seq tl

let print_func func =
  print_typ func.return;
  printf "%s(" func.name;
  print_params func.params;
  printf "){\n"; 
  incr num_tabs;
  print_vars func.locals;
  print_seq func.code;
  decr num_tabs;
  printf "}\n"

let rec print_funcs funcs =
  match funcs with
    | [] -> ()
    | hd::tl ->
      print_func hd;
      print_newline ();
      print_funcs tl

let print_prog prog =
  print_structs prog.structs;
  print_vars prog.globals;
  print_funcs prog.functions

let print_expr_err expr =
  print_newline ();
  print_expr expr;
  print_newline ();
  print_string "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"

let print_instr_err instr =
  print_newline ();
  print_instr instr;
  print_newline ();
  print_string "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"

