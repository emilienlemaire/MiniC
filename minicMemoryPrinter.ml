open MinicInterpreterTypes

let string_of_bool b =
  if b = 0 then
    "true"
  else
    "false"

let rec string_of_var: var -> string = function
  | Int(n) -> "Int("^(string_of_int n)^")"
  | Bool(b) -> "Bool("^(string_of_bool b)^")"
  | Struct(vars) -> "Struct("^(
      List.fold_left (fun acc var ->
          let (name, address) = var in
          acc^name^": @"^(string_of_int address)^", "
        ) "" vars
    )^")"
  | Ptr(n) -> "Prt(@"^string_of_int n^")"
  | InitList(vars) -> "InitList("^(
      List.fold_left (fun acc var ->
          acc^(string_of_var var)^", "
        ) "" vars
    )^")"
  | NamedInitList(vars) -> "NamedInitList("^(
      List.fold_left (fun acc var ->
          let (name, var_val) = var in
          acc^name^": @"^(string_of_var var_val)^", "
        ) "" vars
    )^")"
  | Null -> "Null"

let print_mem (mem:memory) : unit =
  let l = List.sort (fun (key1, _) (key2, _) ->
      compare key1 key2
    ) (Hashtbl.fold (fun key var acc -> acc@[key, var]) mem [])
  in
  List.iter (fun (address, var) ->
      Printf.printf "%d: %s\n" address (string_of_var var)
    ) l

let print_mem_map: memory_map -> unit =
  Hashtbl.iter (fun name addres ->
      Printf.printf "%s: @%d\n" name addres
    )

let print_frame ({mem=mem; mem_map=mem_map; _}: env): unit =
  print_string "________________Memory Map_______________\n";
  print_mem_map mem_map;
  print_string "\n________________Memory__________________\n";
  print_mem mem;
  print_newline ()
