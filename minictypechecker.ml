(* MiniC - Emilien Lemaire - Projet de Compilation
 * Fichier: minicTypeChecker.ml
 * Fichier contenant toutes les fonctions qui permettent de verifier
 * les types de toutes les expressions et fonctions.
 *
 * Toutes les fonctions commençant par `get_` renvoient une valeur typ.
 * Toutes les fonctions commençant par `check_` renvoient une valeur context,
 * sauf `check_prog` qui renvoient unit.
 * *)

open MinicAstTypes
open Printf
module P = MinicPrinter

exception TypeError of string
exception Unreachable
exception NotImplemented
exception NotDefined of string

(* Type: prototype
 * Ce type permet de représenter les prototype des fonctions MiniC
 * *)
type prototype = string * (typ * typ list)

(* Type: context
 * Ce type a été créer afin d'avoir le contexte dans lequel on doit vérifier les
 * tpyes. Contrairement à l'environnement, ce type ne contient pas de valeurs associées aux
 * variables.
 * *)
type context =
  {
    structs: (string * (string * typ) list) list;
    vars: (string * typ) list;
    funcs: prototype list;
    current_ret_type: typ option;
  }

let member_num = ref 0

(* Quelques fonctions d'aides afin d'afficher les erreurs de type.
 * *)
let rec string_of_typ = function
  | Bool -> "bool"
  | Int -> "int"
  | Void -> "void"
  | Struct (n, _) -> Printf.sprintf "struct %s" n
  | Ptr(typ) -> Printf.sprintf "Pointer to %s" (string_of_typ typ)

let string_list_of_typs_list =
  List.fold_left (fun acc typ ->
      acc@[string_of_typ typ]
    ) []

let make_prototype (func: func_def): prototype =
  let (_ , params_typ) = List.split func.params in
  (func.name, (func.return, params_typ))

(* 
 * Fonction d'aide qui permet de vérifier si deux typ sont équivalents
 * Elle sert à vérifier notament si une liste d'initialisation est
 * compatible avec une struct.
 * *)
let rec equal_type (typ1: typ) (typ2: typ): bool =
  match typ1, typ2 with
  | Struct(name1, members1), Struct(name2, members2) ->
    if name1 = name2 then
      true
    else
    if (name1 = "anon") || (name2 = "anon") then
      let (_, members1_typs), (_, members2_typs) =
        (List.split members1, List.split members2)
      in
      try List.for_all2 equal_type members1_typs members2_typs
      with Invalid_argument _ -> raise ( TypeError (
          sprintf "Invalid numbers of elements between two structs %s has %d and %s has %d."
            name1
            (List.length members1)
            name2
            (List.length members2)
        ))
    else
      false
  | Ptr(typ1), Ptr(typ2) -> equal_type typ1 typ2
  | _ -> typ1 = typ2

let get_var_typ (name:string) (vars:(string * typ) list): typ =
  try List.assoc name vars
  with Not_found ->
    raise (NotDefined 
             (sprintf "Trying to access the variable %s which was never defined"
                name
             )
          )

let get_member_typ (struct_type: string) (member: string) (struct_members: (string * typ) list): typ =
  try List.assoc member struct_members
  with Not_found ->
    raise (NotDefined (
        sprintf "Member %s is not in strcut type %s"
          member
          struct_type
      ))

let get_func_ret_typ ({funcs=funcs;_}: context) (name: string): typ =
  let (ret_typ, _) = try List.assoc name funcs
    with Not_found -> raise ( NotDefined (
        sprintf "Function not defined: %s"
          name
      ))
  in
  ret_typ

let rec get_args_typs (ctx:context): expr list -> typ list = 
  List.fold_left ( fun acc expr ->
      acc@[get_expr_typ ctx expr]
    ) []

and get_anon_members_typs (ctx: context) (members: expr list): (string * typ) list =
  List.fold_left (fun acc member_expr ->
      incr member_num;
      acc@["__m"^(string_of_int (!member_num)), (get_expr_typ ctx member_expr)]
    ) [] members

and get_access_typ (ctx: context): expr -> typ = function
  | Get(n) -> get_var_typ n ctx.vars
  | StructMember (access, member) as mbr ->
    (
      let typ = get_access_typ ctx access in
      match typ with
      | Struct(name, members) -> get_member_typ name member members
      | Ptr(Struct(_)) -> raise ( TypeError (
          P.print_expr_err mbr;
          sprintf
            "Trying to access member %s of a pointer to a struct with '.', did you mean '->'?\n"
            member
        ))
      | _ -> raise ( TypeError (
          P.print_expr_err mbr;
          sprintf
            "Trying to access member %s from a non struct type %s."
            member
            (string_of_typ typ)
        ))
    )
  | StructPtrMember(access, member) as ptr ->
    (
      let typ = get_access_typ ctx access in
      match typ with
      | Ptr(Struct(name, members)) -> get_member_typ name member members
      | Struct(_) -> raise ( TypeError (
          P.print_expr_err ptr;
          sprintf
            "Trying to access member %s of a struct with '->', did you mean '.'?\n"
            member
        ))
      | _ -> raise ( TypeError (
          P.print_expr_err ptr;
          sprintf
            "Trying to access member %s from a non struct type %s."
            member
            (string_of_typ typ)
        ))
    )
  (* Pour les operations de ce type *(p + 1) = qqch où p est un pointer vers une variable de
   * même type que qqch
   * *)
  | Deref(BinOp((Plus|Minus), Get(n), Cst _)) -> get_access_typ ctx (Deref(Get(n)))
  | Deref( access ) ->
    (
      let typ = get_access_typ ctx access in
      match typ with
      | Ptr(typ') -> typ'
      | _ -> raise ( TypeError (
          P.print_expr_err access;
          sprintf "Trying to dereference a variable of type %s which is not a pointer type."
            (string_of_typ typ)
        ))
    )
  | Call(name, _) -> get_func_ret_typ ctx name
  | e -> raise ( TypeError (
      P.print_expr_err e;
      "Trying to access an invalid access expression."
    ))
and get_binop_typ (ctx: context) (lhs: expr) (rhs: expr): binop -> typ = function
  | Plus | Minus | Times | By | Mod as binop->
    let typ1 = get_expr_typ ctx lhs in
    let typ2 = get_expr_typ ctx rhs in
    begin
      match typ1, typ2 with
      | Int, Int -> Int
      | Ptr typ, Int -> Ptr typ
      | _ -> raise ( TypeError (
          P.print_expr_err (BinOp (binop, lhs, rhs));
          sprintf
            "Operands of types %s and %s are incompatible with arithmetcs operations"
            (string_of_typ typ1)
            (string_of_typ typ2)
        )
        )
    end
  | Lth | Gth | Leq | Geq
  | Eq  | Neq | And | Or as binop ->
    let typ1 = get_expr_typ ctx lhs in
    let typ2 = get_expr_typ ctx lhs in
    begin
      match typ1, typ2 with
      | Int, Int
      | Int, Bool
      | Bool, Int
      | Bool, Bool -> Bool
      | _ -> raise ( TypeError (
          P.print_expr_err (BinOp (binop, lhs, rhs));
          sprintf
            "Operands of types %s and %s are incompatible with logic operations"
            (string_of_typ typ1)
            (string_of_typ typ2)
        ))
    end

and get_expr_typ (ctx: context): expr -> typ = function
  | Cst(_) -> Int
  | BinOp(binop, lhs, rhs) -> get_binop_typ ctx lhs rhs binop
  | Get(name) -> get_var_typ name ctx.vars
  | Not(expr) as e ->
    begin
      let expr_typ = get_expr_typ ctx expr in
      match expr_typ with
      | Bool | Int -> Bool
      | _ ->
        raise (TypeError (
            P.print_expr_err e;
            sprintf "Trying to negate an expression of type %s. Expecting a boolean or an integer.\n"
              (string_of_typ expr_typ)
          ))
    end
  | Neg(expr) as e -> 
    begin
      let expr_typ = get_expr_typ ctx expr in
      if (equal_type expr_typ Int) then
        Int
      else
        raise (TypeError (
            P.print_expr_err e;
            sprintf "Trying to inverse the sign of an expression of type %s. Expecting an integer."
              (string_of_typ expr_typ)
          ))
    end
  | Call(name, args) as c ->
    let (ret_typ, params_typs) =
      try List.assoc name ctx.funcs 
      with Not_found ->
        raise ( NotDefined (
            P.print_expr_err c;
            sprintf "Call to an undefined function: %s\n"
              name
          ))
    in
    let args_typs = get_args_typs ctx args in
    let good_args =
      begin
        try List.for_all2 equal_type params_typs args_typs
        with Invalid_argument _ ->
          raise (TypeError (
              P.print_expr_err c;
              sprintf "Invalide number of arguments when calling %s, expecting %d instead of %d.\n"
                name
                (List.length params_typs)
                (List.length args_typs))
            )
      end
    in
    if good_args then
      ret_typ
    else
      raise (TypeError (
          P.print_expr_err c;
          sprintf "Wrong arguments types when calling %s, got (%s) instead of (%s)"
            name
            (String.concat ", " (string_list_of_typs_list args_typs))
            (String.concat ", " (string_list_of_typs_list params_typs))
        ))
  | BoolLit(_) -> Bool
  | Address(expr) -> Ptr(get_expr_typ ctx expr)
  | InitList(expr_list) -> Struct("anon", get_anon_members_typs ctx expr_list)
  (*| StructMember | StructPtrMember | Deref *)
  | access -> get_access_typ ctx access

let rec check_instr (ctx: context): instr -> context = function
  | Putchar(expr) as p ->
    (match get_expr_typ ctx expr with
      | Bool
      | Int -> ctx
      | t -> raise (TypeError (
          P.print_instr_err p;
          sprintf "Trying to print a value of type %s, expecting bool or int."
            (string_of_typ t)
        ))
    )
  | Set(expr1, expr2) as set ->
    let access_typ = get_access_typ ctx expr1 in
    let expr_typ   = get_expr_typ ctx expr2 in
    if (equal_type access_typ expr_typ) then
      ctx
    else
      raise ( TypeError (
          P.print_instr_err set;
          sprintf "Trying to set a value of type %s to a variable of type %s."
            (string_of_typ expr_typ)
            (string_of_typ access_typ)
        ))
  | If(cond, thenb, elseb) as if_instr ->
    let cond_typ = (get_expr_typ ctx cond) in
    if cond_typ = Bool || cond_typ = Int then
      let (ctx, _) = (check_seq ctx thenb, check_seq ctx elseb) in
      ctx
    else
      raise ( TypeError (
          P.print_instr_err if_instr;
          sprintf "The condition of an if block must be a bool or an int, not a %s."
            (string_of_typ cond_typ)
        ))
  | While(cond, whileb) as while_instr ->
    let cond_typ = get_expr_typ ctx cond in
    if cond_typ = Bool || cond_typ = Int then
      check_seq ctx whileb
    else
      raise ( TypeError (
          P.print_instr_err while_instr;
          sprintf "The condition of an if block must be a bool or an int, not a %s."
            (string_of_typ cond_typ)
        ))
  | Return(expr) as return ->
    (match ctx.current_ret_type, (get_expr_typ ctx expr) with
      | Some(ret_typ), expr_type ->
        if equal_type ret_typ expr_type then
          ctx
        else
          raise ( TypeError (
              P.print_instr_err return;
              sprintf "Expecting a return type of %s instead of %s."
                (string_of_typ ret_typ)
                (string_of_typ expr_type)
            ))
      | _ -> raise Unreachable)
  | Expr(expr) ->
    let (_: typ) = get_expr_typ ctx expr in
    ctx
and check_seq (ctx: context): seq -> context =
  List.fold_left (check_instr) ctx

let check_func (ctx: context) (func: func_def): context =
  let ctx' = {
    structs= ctx.structs;
    vars= (ctx.vars@func.locals@func.params);
    funcs= (make_prototype func)::ctx.funcs;
    current_ret_type= Some(func.return);
  }
  in
  let (_ : context) = check_seq ctx' func.code in
  {
    structs= ctx.structs;
    vars= ctx.vars;
    funcs= ctx'.funcs;
    current_ret_type=None;
  }

let check_funcs (ctx: context): func_def list -> context =
  List.fold_left check_func ctx

let check_prog (prog: prog) =
  let ctx = {
    structs= prog.structs;
    vars= prog.globals;
    funcs= [];
    current_ret_type= None;
  }
  in
  let (_: context) = check_funcs ctx prog.functions in
  ()

