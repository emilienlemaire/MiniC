open Minictypechecker
open Ast_types

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

let _ = 
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let prog = Minicparser.prog Miniclexer.token lexbuf in
  let env = {
    vars = prog.globals;
    funcs = [];
    current_ret_type = Void;
  } in
  let _ = check_funcs prog.functions env in
  Printf.printf "Validated program\n";
  ()
