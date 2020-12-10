exception Redefinition of string
exception Unreachable
exception MissingMain

type var =
  | Int of int
  | Bool of bool
  | Struct of (string * var) list
  | Ptr of var

let rec var_of_typ = function
  | Ast_types.Int -> Int 0
  | Ast_types.Bool -> Bool false
  | Ast_types.Struct(_, members) -> build_struct members
  | Ast_types.Ptr(typ) -> build_ptr typ
  | Ast_types.Void -> raise Unreachable
and build_struct members =
  let rec loop members acc =
    match members with
      | [] -> acc
      | (name, typ)::tl ->
        loop tl (acc@[(name, var_of_typ typ)])
  in
  Struct (loop members [])
and build_ptr ptr =
  Int 0

let add_var name typ env =
  if Hashtbl.mem env name then
    raise (Redefinition (Printf.sprintf "Redefinition of variable %s.\n" name))
  else
    match typ with
      | Ast_types.Int -> Hashtbl.add env name (Int 0)
      | Ast_types.Bool -> Hashtbl.add env name (Bool false)
      | Ast_types.Struct( _ ) -> Hashtbl.add env name (var_of_typ typ)
      | Ast_types.Ptr ( _ ) -> Hashtbl.add env name (var_of_typ typ)
      | Ast_types.Void -> raise (Invalid_argument "A variable should not have a pointer type.")

let rec add_globals globals env =
  match globals with
    | [] -> env
    | (name, typ)::tl ->
      add_var name typ env;
      add_globals tl env

let init_env (prog: Ast_types.prog) =
  let env = Hashtbl.create 100 in
  let env' = add_globals prog.globals env in
  env'

let make_func_stack (funcs: Ast_types.func_def list) =
  let stack =
    List.fold_left (fun acc (f: Ast_types.func_def) ->
        Stack.push (f.name, f) acc;
        acc
      ) (Stack.create ()) funcs
  in
  stack


let rec find_main func_stack =
  let func = Stack.pop_opt func_stack in
  match func with
    | Some(("main", f)) -> (f, func_stack)
    | Some(_) -> find_main func_stack
    | None -> raise MissingMain

let interpret_func main_func func_stack env =
  ()

let interpret_prog prog =
  let env = init_env prog in
  let func_stack = make_func_stack prog.functions in
  let (main_func, func_stack) = find_main func_stack in
  let _ = interpret_func main_func func_stack env in
  ()


