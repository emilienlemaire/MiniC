module H = Hashtbl
module AST = MinicAstTypes

type var =
  | Int of int
  | Bool of int
  | Struct of (string * int) list
  | InitList of var list
  | NamedInitList of (string * var) list
  | Ptr of int
  | Null

type memory = (int, var) H.t
type memory_map = (string, int) H.t

type env = {
  mem: memory;
  mem_map: memory_map;
  funcs: (string * AST.func_def) list;
  call_stack: string Stack.t;
  ret_var: var;
}

module InterpreterOp = struct
  let not a =
    if a <> 0 then
      0
    else
      1

  let (<) a b =
    if Stdlib.(<) a b then
      1
    else
      0

  let (>) a b =
    if Stdlib.(>) a b then
      1
    else
      0

  let (<=) a b =
    if Stdlib.(<=) a b then
      1
    else
      0

  let (>=) a b =
    if Stdlib.(>=) a b then
      1
    else
      0

  let (=) a b =
    if Stdlib.(=) a b then
      1
    else
      0

  let (<>) a b =
    if Stdlib.(<>) a b then
      1
    else
      0

  let (&&) a b =
    if Bool.(&&) (Stdlib.(<>) a 0) (Stdlib.(<>) b 0) then
      1
    else
      0

  let (||) a b =
    if Bool.(||) (Stdlib.(<>) a 0) (Stdlib.(<>) b 0) then
      1
    else
      0
end

