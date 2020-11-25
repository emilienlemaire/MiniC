type expr =
  | Cst     of int
  | Add     of expr * expr
  | Mul     of expr * expr
  | Lth     of expr * expr
  | Gth     of expr * expr
  | Leq     of expr * expr
  | Geq     of expr * expr
  | Eq      of expr * expr
  | Neq     of expr * expr
  | Get     of string
  | Call    of string * expr list
  | BoolLit of bool

type instr =
  | Putchar of expr
  | Set     of string * expr
  | If      of expr * seq * seq
  | While   of expr * seq
  | Return  of expr
  | Expr    of expr
and seq = instr list

type typ =
  | Int
  | Bool
  | Void

type func_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code: seq;
}

type prog = {
  globals: (string * typ) list;
  functions: func_def list;
}

