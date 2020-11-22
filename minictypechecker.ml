open Ast_types

exception TypeError

type context =
  {
    vars: (string * typ) list;
    funcs: (string * (typ * typ list)) list;
    current_ret_type: typ;
  }

let type_to_string t =
  match t with
    | Bool -> "bool"
    | Int -> "int"
    | Void -> "void"

let string_of_argt argst =
  let rec loop argst acc =
    match argst with
      | [] -> acc
      | hd::tl -> loop tl (String.concat " * " [acc; (type_to_string hd)])
  in
  let fstArg = type_to_string (List.hd argst) in
  let str = String.concat "" ["("; fstArg] in
  let str = loop (List.tl argst) str in
  let str = String.concat "" [str; ")"] in
  str

let rec check_expr expr env =
  match expr with
    | Cst (_) -> Int
    | Add (e1, e2) ->
        let t1 = check_expr e1 env in
        let t2 = check_expr e2 env in
        if t1 <> Int || t2 <> Int then
          begin
            Printf.printf 
              "Type error: expression has type (%s * %s) instead of (Int * Int)\n"
              (type_to_string t1)
              (type_to_string t2);
            raise TypeError
          end
        else
          Int
      | Mul (e1, e2) ->
        let t1 = check_expr e1 env in
        let t2 = check_expr e2 env in
        if t1 <> Int || t2 <> Int then
          begin
            Printf.printf 
              "Type error: expression has type (%s * %s) instead of (Int * Int)\n"
              (type_to_string t1)
              (type_to_string t2);
            raise TypeError
          end
        else
          Int
      | Lth (e1, e2) ->
        let t1 = check_expr e1 env in
        let t2 = check_expr e2 env in
        if t1 <> Int || t2 <> Int then
          begin
            Printf.printf 
              "Type error: expression has type (%s * %s) instead of (Int * Int)\n"
              (type_to_string t1)
              (type_to_string t2);
            raise TypeError
          end
        else
          Bool
      | Get (n) -> List.assoc n (env.vars)
      | Call (n, args) -> 
        let proto = List.assoc n env.funcs in
        let (ret, argt) = proto in
        let (good, types) = check_args argt args env in
        if not good then
          begin
            Printf.printf
              "Invalid arguments type for function %s: expected %s instead of %s\n"
              n
              (string_of_argt argt)
              (string_of_argt types)
            ;
              raise TypeError
          end
        else
          ret
      | BoolLit (_) -> Bool
and check_args argst args env =
  let rec loop argst args res =
    match args with
      | [] -> res
      | hd::tl ->
        let (resb, resl) = res in
        let expectedt = List.hd argst in
        let actualt = check_expr hd env in
        if expectedt <> actualt then
          loop (List.tl argst) tl (false, resl@[actualt])
        else
          loop (List.tl argst) tl (resb, resl@[actualt])
  in
  loop argst args (true, [])

let rec check_instr instr env =
  match instr with
    | Putchar (_) -> Void
    | Set (n, e) ->
      let vart = List.assoc n (env.vars) in
      let exprt = check_expr e env in
      if vart <> exprt then
        begin
          Printf.printf
            "Bad type assignement for %s: expected %s instead of %s.\n"
            n
            (type_to_string vart)
            (type_to_string exprt);
          raise TypeError
        end
      else
        Void
    | If (cond, then_seq, else_seq) -> 
      if (check_expr cond env) <> Bool then
        begin
          Printf.printf
            "The condition expect type bool instead of %s\n"
            (type_to_string (check_expr cond env));
          raise TypeError;
        end
      else
        begin
          let _ = check_seq then_seq env in
          let _ = check_seq else_seq env in
          Void
        end
    | While (cond, seq) ->
      if (check_expr cond env) <> Bool then
        begin
          Printf.printf
            "The condition expect type bool instead of %s\n"
            (type_to_string (check_expr cond env));
          raise TypeError;
        end
      else
        begin
          let _ = check_seq seq env in
          Void
        end
    | Return (e) ->
      let exprt = check_expr e env in
      if exprt <> env.current_ret_type then
        begin
          Printf.printf
            "Expected return type of %s instead of %s.\n"
            (type_to_string env.current_ret_type)
            (type_to_string exprt);
          raise TypeError
        end
      else
        env.current_ret_type
    | Expr (e) -> (check_expr e env)
and check_seq seq env =
  match seq with
    | [] -> Void
    | hd::tl ->
        let _ = check_instr hd env in
        check_seq tl env

let check_func func env =
  let (_, paramst) = List.split func.params in
  let proto = (func.name, (func.return, paramst)) in
  let env2 = {
    vars= env.vars@func.params@func.locals;
    funcs= env.funcs@[proto];
    current_ret_type = func.return;
  } in
  let _ = check_seq func.code env2 in
  {
    vars= env.vars;
    funcs= env2.funcs;
    current_ret_type = env.current_ret_type;
  }

let rec check_funcs (funcs: func_def list) (env: context) =
  match funcs with
    | [] -> env
    | hd::tl ->
      check_funcs tl (check_func hd env)

