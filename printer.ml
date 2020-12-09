open Printf
open Ast_types

let num_tabs = ref 0

let rec print_tabs n =
  if n > 0 then
    begin
      printf "  ";
      print_tabs (n-1)
    end
  else
    ()

let rec print_ptr typ acc =
  match typ with
    | Ptr(typ) -> print_ptr typ ("*"^acc)
    | _ -> print_typ typ; printf "%s" acc

and print_typ typ =
  match typ with
    | Int -> printf "int "
    | Bool -> printf "bool "
    | Void -> printf "void "
    | Struct(name, _) -> printf "struct %s " name
    | Ptr(_) -> print_ptr typ ""

let rec print_members members =
  match members with
    | [] -> ()
    | (name, typ)::tl ->
      begin
        print_tabs (!num_tabs);
        print_typ typ;
        printf "%s;" name;
        print_newline ();
        print_members tl
      end

let print_struct s =
  match s with
    | Struct(name, members) ->
      begin
        print_tabs (!num_tabs);
        printf "struct %s {\n" name;
        incr num_tabs;
        print_members members;
        decr num_tabs;
        print_tabs (!num_tabs);
        printf "};\n";
      end
    | _ -> raise (Invalid_argument "Unreachable")

let rec print_structs structs =
  match structs with
  | [] -> print_newline ()
  | (name, members)::tl ->
    print_struct (Struct(name, members));
    print_newline ();
    print_structs tl

