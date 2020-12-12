open Printf
open MinicInterpreterTypes
open MinicAstTypes

module H = Hashtbl
module MP = MinicMemoryPrinter
module IOp = InterpreterOp

exception Redefinition of string
exception Unreachable of string
exception MissingMain
exception NotImplementedYet
exception BadPointerAccess of string

let ptr_address = ref 0

let get_free_address ({mem=mem;_}: env): int =
  let rec loop mem address =
    if H.mem mem address then
      loop mem (address+1)
    else
      address
  in
  loop mem 0

let rec make_struct (env: env) (members: (string * typ) list): (string * int) list =
  List.fold_left (fun acc (name, typ) ->
      let address = get_free_address env in
      H.add env.mem address (Null); 
      H.replace env.mem address (make_var env typ); 
      acc@[name, address]
    ) [] members

and make_ptr (env: env): typ -> var = function
  | Ptr(typ) -> 
    let address = get_free_address env in
    H.add env.mem (address) (Null);
    H.replace env.mem (address) (Ptr(get_free_address env));
    make_ptr env typ
  | Struct(_, members) ->
    let address = get_free_address env in
    H.add env.mem (address) (Null);
    H.replace env.mem address (Struct(make_struct env members));
    Ptr(!ptr_address)
  | _ ->
    let address = get_free_address env in
    H.add env.mem (address) (Null);
    Ptr(!ptr_address)


and make_var (env: env): typ -> var = function
  | Int -> Int(0)
  | Bool -> Bool(0)
  | Struct(_, members) -> Struct(make_struct env members)
  | Ptr(typ) ->
    ptr_address := get_free_address env;
    make_ptr env typ
  | Void -> Null

let add_var (env: env) ((name, typ): string * typ): env =
  let address = get_free_address env in
  H.add env.mem_map name address;
  H.add env.mem address (Null);
  let var = make_var env typ in
  H.replace env.mem address var;
  env

let add_vars (env: env): (string * typ) list -> env =
  List.fold_left (fun env global ->
      add_var env global
    ) env

let add_funcs (env:env): func_def list -> env =
  List.fold_left (fun env func ->
      {
        mem=env.mem;
        mem_map=env.mem_map;
        funcs=env.funcs@[func.name, func];
        call_stack= env.call_stack;
        ret_var= env.ret_var;
      }
    ) env

let rec set_var (env: env) ((name, var): (string * var)): env =
  set_var_at_address env (H.find env.mem_map name) var

and set_var_at_address (env: env) (address: int): var -> env = function
  | InitList(vars) as il ->
    (match H.find env.mem address with
      | Struct(addresses) as s ->
        List.fold_left2 ( fun env (_, address) var ->
          set_var_at_address env address var
        ) env addresses vars
      | _ as v -> raise (Unreachable 
                     (
                      sprintf "set_var_at_address expecting %s instead of %s."
                      (MP.string_of_var il)
                      (MP.string_of_var v)
                    )
                   )
    )
  | _ as var -> H.replace env.mem address var; env

let remove_locals (env: env) (func: func_def): env =
  let rm = List.fold_left (fun env (var, _) ->
      let address = H.find env.mem_map var in
      H.remove env.mem address;
      H.remove env.mem_map var;
      env
    ) in
  rm env (func.params@func.locals)

let init_env (prog: prog): env =
  let env = {
    mem= H.create 100;
    mem_map= H.create 100;
    funcs = [];
    call_stack= Stack.create ();
    ret_var= Null;
  } in
  let env' = add_vars env prog.globals in
  add_funcs env' prog.functions


let get_value (e1: var) (e2: var) (op: int -> int -> int): int =
  match e1, e2 with
    | Bool(b1), Bool(b2) -> op b1 b2
    | Bool(b), Int(i) -> op b i
    | Int(i), Bool(b) -> op i b
    | Int(i1), Int(i2) -> op i1 i2
    |_ ->  raise (Unreachable "get_value")

let rec get_val_of_access (env:env): expr -> var = function
  | Get(name) ->
    let address = H.find env.mem_map name in
    H.find env.mem address
  | StructMember(access, member) ->
    let struct_var = get_val_of_access env access in
    begin
      match struct_var with
      | Struct(members) ->
        let address = try List.assoc member members
        with Not_found -> raise (Unreachable (sprintf "Not found struct member %s" member)) in
        H.find env.mem address
      | NamedInitList(vars) ->
        (try List.assoc member vars
         with Not_found -> raise (Unreachable (sprintf "Not found init list member %s" member))
        )
      | _ as s -> raise (Unreachable (
          sprintf "\n get_val_of_access %s instead of %s"
            (MP.string_of_var s)
            (MP.string_of_var struct_var)
        ))
    end
  | StructPtrMember(access, member) ->
    let ptr = get_val_of_access env access in
    (match ptr with
      | Ptr(address) ->
        (match H.find env.mem address with
          | Struct(members) ->
            let address = try List.assoc member members
              with Not_found -> raise (Unreachable (sprintf "Not found struct member %s" member)) in
            H.find env.mem address
          | _ -> raise (Unreachable "get_val_of_access structptrmember")
        )
      | _ -> raise (Unreachable "get_val_of_access structptrmember not ptr")
    )
  | Deref(BinOp((Plus|Minus), Get(n), Cst offset)) ->
    let address = ((H.find env.mem_map n) + offset) in
    if H.mem env.mem address then
      H.find env.mem address
    else
      raise (BadPointerAccess (sprintf "Address: %d" address))
  | Deref(access) ->
    let ptr = get_val_of_access env access in
    (match ptr with
      | Ptr(address) -> H.find env.mem address
      | _ -> raise (Unreachable "get_val_of_access deref")
    )
  | Call(name, args) -> eval_call env name args
  | _ -> raise (Unreachable "get_val_of_access other")

and get_address_of_access (env:env): expr -> int = function
  | Get(name) ->
    H.find env.mem_map name
  | StructMember(access, member) ->
    let struct_var = get_val_of_access env access in
    begin
      match struct_var with
      | Struct(members) -> ( try List.assoc member members
                             with Not_found -> raise (Unreachable (sprintf "Not found struct member %s" member))
                           )
      | _ -> raise (Unreachable "get_address_of_access struct member")
    end
  | StructPtrMember(access, member) ->
    let ptr = get_val_of_access env access in
    (match ptr with
      | Ptr(address) ->
        (match H.find env.mem address with
          | Struct(members) -> ( try List.assoc member members
                             with Not_found -> raise (Unreachable (sprintf "Not found struct member %s" member))
                           )
          | _ -> raise (Unreachable "get_address_of_access structptrmember")
        )
      | _ -> raise (Unreachable "get_address_of_access structptrmember not ptr")
    )
  | Deref(BinOp((Plus|Minus), Get(n), Cst offset)) ->
    let address = ((H.find env.mem_map n) + offset) in
    if H.mem env.mem address then
      address
    else
      raise (BadPointerAccess (sprintf "Address: %d" address))
  | Deref(access) ->
    let ptr = get_val_of_access env access in
    (match ptr with
      | Ptr(address) -> address
      | _ -> raise (Unreachable "get_address_of_access deref")
    )
  | _ -> raise (Unreachable "get_address_of_access other")

and eval_expr (env: env): expr -> var = function
  | Cst(n) -> Int(n)
  | BinOp(op, e1, e2) -> eval_binop env e1 e2 op
  | Get(name) -> let address = H.find env.mem_map name in
    H.find env.mem address
  | Not(e) ->
    (match eval_expr env e with
      | Bool(b) -> Bool(IOp.not b)
      | _ -> raise (Unreachable "eval_expr not not bool")
    )
  | Neg(e) ->
    (match eval_expr env e with
      | Int(n) -> Int(-n)
      | _ -> raise (Unreachable "eval_expr neg not int")
    )
  | Call(func_name, args_list) -> eval_call env func_name args_list
  | BoolLit(b) -> Bool(if b then 1 else 0)
  | StructMember(_)
  | StructPtrMember(_)
  | Deref(_) as access -> get_val_of_access env access
  | Address(access) -> Ptr(get_address_of_access env access)
  | InitList(expr_list) -> InitList(List.fold_left (fun acc expr ->
      acc@[eval_expr env expr]
    ) [] expr_list)

and eval_call (env: env) (name: string) (args: expr list): var =
  let func_def = try List.assoc name env.funcs
    with Not_found ->
      raise (Unreachable "eval_call func not found")
  in
  let calling_func = List.assoc (Stack.top env.call_stack) env.funcs in
  Stack.push name env.call_stack;
  let (args_name, _) = List.split func_def.params in
  let args_var = List.fold_left (fun acc arg ->
      acc@[eval_expr env arg]
    ) [] args
  in
  let env = add_vars env (func_def.params@func_def.locals) in
  let env = List.fold_left (set_var) env (List.combine args_name args_var) in
  let env = interpret_seq env func_def.code in
  let env = remove_locals env func_def in
  let _ = Stack.pop env.call_stack in
  match func_def.return with
  | Struct(_, members) ->
    (match env.ret_var with
     | InitList(vars) -> NamedInitList(
         List.fold_left2 (fun acc (name, _) var ->
            acc@[name, var]
           ) [] members vars
       )
      | Struct(_) -> env.ret_var
      | _ -> raise (Unreachable "eval_call")
    )
  | _ -> env.ret_var

and eval_binop (env:env) (e1:expr) (e2: expr) (op: binop): var =
  let var1 = eval_expr env e1 in
  let var2 = eval_expr env e2 in
  match op with
  | Plus -> Int(get_value var1 var2 (+))
  | Minus -> Int(get_value var1 var2 (-))
  | Times -> Int(get_value var1 var2 ( * ))
  | By -> Int(get_value var1 var2 (/))
  | Mod -> Int(get_value var1 var2 (mod))
  | Lth -> Bool(get_value var1 var2 IOp.(<))
  | Gth -> Bool(get_value var1 var2 IOp.(>))
  | Leq -> Bool(get_value var1 var2 IOp.(<=))
  | Geq -> Bool(get_value var1 var2 IOp.(>=))
  | Eq -> Bool(get_value var1 var2 IOp.(=))
  | Neq -> Bool(get_value var1 var2 IOp.(<>))
  | And -> Bool(get_value var1 var2 IOp.(&&))
  | Or -> Bool(get_value var1 var2 IOp.(||))

and interpret_instr (env:env): instr -> env = function
  | Putchar(e) -> (
      match eval_expr env e with
        | Int(n) -> print_int n; print_newline (); env
        | Bool(b) -> print_string ((MP.string_of_bool b)^"\n"); env
        | _ -> raise (Unreachable "interpret_instr putchar not int bool")
    )
  | Set(access, expr) as i ->
    set_var_at_address env (get_address_of_access env access) (eval_expr env expr)
  | If(cond, thenb, elseb) ->
    (match eval_expr env cond with
      | Int(n) | Bool(n) -> if n <> 0 then
        interpret_seq env thenb
        else
          interpret_seq env elseb
      | _ -> raise (Unreachable "interpret_instr if cond not bool")
    )
  | While(cond, seq) as whilei ->
    (match eval_expr env cond with
      | Int(n) | Bool(n) -> if n <> 0 then
        interpret_seq env (seq@[whilei])
        else
          env
      | _ -> raise (Unreachable "interpret_instr while cond not bool")
    )
  | Return expr ->
    let ret_val = eval_expr env expr in
    {
      mem=env.mem;
      mem_map=env.mem_map;
      funcs=env.funcs;
      call_stack=env.call_stack;
      ret_var= ret_val;
    }
  | Expr e ->
    let _ = eval_expr env e in
    env

and interpret_seq (env: env): seq -> env =
  List.fold_left (fun env instr ->
      interpret_instr env instr
    ) env

let interpret_func (env: env) (func: func_def): env =
  let env' = interpret_seq 
    (add_vars env (func.locals@func.params)) func.code
  in
  env'

let interpret (prog: prog): unit =
  let env = init_env prog in
  let main_func = try List.assoc "main" env.funcs
    with Not_found -> raise MissingMain
  in
  Stack.push "main" env.call_stack;
  let env = interpret_func env main_func in
  ()
