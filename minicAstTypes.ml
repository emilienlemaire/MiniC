(* MiniC - Emilien Lemaire - Projet de Compilation
 * Fichier: ast_types.ml
 *  Ce fichier contient tous les types utilisés pour générer l'abre de syntaxe
 *  ainsi que les types de variables de Minic.
 * Notes:
 *  - Chaque ajout effectué par rapport au type de base donné dans l'énoncé du DM
 *      est expliqué en commentaire.
 * *)

type binop =
  | Plus
  | Minus
  | Times
  | By
  | Mod
  | Lth
  | Gth
  | Leq
  | Geq
  | Eq
  | Neq
  | And
  | Or

(*
 * Ajouts:
 *  - BoolLit afin de pouvoir repérer les expression 'true' et ' false'
 *  - StructMember afin de pouvoir repéré les expressions de la forme myStruct.theMember
 *  - StructPtrMember afin de pouvoir accéder aux membres des pointeurs de structure
 *  - Deref afin de déréférencer les pointeurs
 *  - Address afin d'accéder aux adresse des variables
 *  - InitList afin de représenter les listes d'initialisations
 * Modifications:
 *  - J'ai ajouté le constructeur BinOp afin d'avoir un seul constructeur
 *     pour toutes les opérations binaires.
 * *)
type expr =
  | Cst             of int
  | BinOp           of binop * expr * expr
  | Get             of string
  | Not             of expr
  | Neg             of expr
  | Call            of string * expr list
  | BoolLit         of bool
  | StructMember    of expr * string
  | StructPtrMember of expr * string
  | Deref           of expr
  | Address         of expr
  | InitList        of expr list

(*
 * Modifications:
 *  - Set est devenu un constructeur de expr * expr, afin de pouvoir avoir
 *     des expressions du type *(a->b) à gauche de l'opérateur '='.
 * *)
type instr =
  | Putchar         of expr
  | Set             of expr * expr
  | If              of expr * seq * seq
  | While           of expr * seq
  | Return          of expr
  | Expr            of expr
and seq = instr list

(*
 * Ajouts:
 *  - Struct: j'ai donné a Struct un constructeur string * (string * typ) list afin de pouvoir
 *     avoir le nom du type structure ainsi que les noms et types de ces membres. La liste est
 *     dans l'ordre des déclarations des membres lors de la définition de la structure.
 *  - Ptr: j'ai donné a Ptr un constructeur typ, afin de pouvoir bien vérifier que les valeurs
 *     pointées sont du bon type dans le vérificateur de type.
 *   *)
type typ =
  | Int
  | Bool
  | Void
  | Struct of string * (string * typ) list
  | Ptr of typ

type func_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code: seq;
}

(*
 * Ajouts:
 *   - structs afin de pouvoir enregistrer toutes les structures créées dans le fichier.
 *      On s'attend a ce que les structures soient déclarées avant les variables globales.
 * *)
type prog = {
  structs: (string * (string * typ) list) list;
  globals: (string * typ) list;
  functions: func_def list;
}

