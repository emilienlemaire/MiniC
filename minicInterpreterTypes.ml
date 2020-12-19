(* MiniC - Emilien Lemaire - DM de Compilation
 * Fichier: minicInterpreterTypes.ml
 * Ce fichier contient tous les types nécessaire à l'interprète.
 * *)
module H = Hashtbl
module AST = MinicAstTypes

(*
 * Ce type sert à représenter les variables dans l'interpète.
 * Le constructeur Null sert à représenter les pointeur dont la valeur n'est
 * pas initialisée.
 * Les Bool sont des int afin de pouvoir les comparer aux int.
 * *)
type var =
  | Int of int
  | Bool of int
  | Struct of (string * int) list
  | InitList of var list
  | NamedInitList of (string * var) list
  | Ptr of int
  | Null

(* 
 * Types pour représenter la mémoire.
 * *)
type memory = (int, var) H.t
type memory_map = (string, int) H.t

(*
 * Type pour représenter l'environnement.
 * *)
type env = {
  mem: memory;
  mem_map: memory_map;
  funcs: (string * AST.func_def) list;
  call_stack: string Stack.t;
  ret_var: var;
}

(*
 * Ce Module permet de faire des comparaisons qui renvoient 1 ou 0
 * plutôt que true ou false. Ceci permet d'avoir des instructions
 * while ou if qui prennent un int dans les conditions.
 * *)
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

