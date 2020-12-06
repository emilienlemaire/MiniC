(* MiniC - Emilien Lemaire _ Projet de Compilation
 * Fichier: minictypecher.ml
 * Fichier contenant toutes les fonctions qui permettent de verifier
 * les types de toutes les expressions et fonctions.
 * 
 * Quelques notes importantes:
 *  - J'ai decide de renvoyer un type Void quand j'analyse une instruction et qu'elle est valide
 *  - Pour verifier les types des structures, lorsque l'on utilise des listes d'initialization,
 *      pour leur assigner une valeur, les type des membres sont verfies les uns apres les autres
 *      suivant l'ordre dans lequel ils ont ete declare dans la definition de la structure.
 *  - 
 *
 * *)

open Ast_types

exception TypeError

(* Type: context
 * Ce type a été créer afin d'avoir le contexte dans lequel on doit vérifier les
 * tpyes. Contrairement à l'environnement, ce type ne contient pas de valeurs associées aux
 * variables.
 * *)
type context =
  {
    structs: (string * (string * typ) list) list;
    vars: (string * typ) list;
    funcs: (string * (typ * typ list)) list;
    current_ret_type: typ;
  }

(* Quelques fonctions d'aides afin d'afficher les erreurs de type.
 * *)
let rec type_to_string t =
  match t with
    | Bool -> "bool"
    | Int -> "int"
    | Void -> "void"
    | Struct (n, _) -> Printf.sprintf "struct %s" n
    | Ptr(typ) -> Printf.sprintf "Pointer to %s" (type_to_string typ)

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

(*
 * Fonction qui permet de vérifier qu'une expression est bien typée.
 * @param expr l'expression a vérifier
 * @param ctx le contexte dans lequel vérifier l'expression
 * @return Le type d'une expression bien typée.
 * *)
let rec check_expr (expr: expr) (ctx:context):typ =
  match expr with
    | Cst (_) -> Int
    | Add (e1, e2)
    | Sub (e1, e2)
    | Mul (e1, e2)
    | Div (e1, e2) ->
      let t1 = check_expr e1 ctx in
      let t2 = check_expr e2 ctx in
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
    | Lth (e1, e2) | Gth(e1, e2)
    | Geq(e1, e2) | Leq(e1, e2)
    | Eq(e1, e2) | Neq(e1, e2) ->
      let t1 = check_expr e1 ctx in
      let t2 = check_expr e2 ctx in
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
    | Get (n) -> List.assoc n (ctx.vars)
    | Neg (expr) ->
      if (check_expr expr ctx) <> Int then
        begin
          Printf.printf
            "Trying to negate a non integer expression.\n"
          ;
          raise TypeError
        end
      else
        Int
    | Not (expr) ->
      if (check_expr expr ctx) <> Bool then
        begin 
          Printf.printf
            "Trying to negate a non bool expression.\n"
          ;
          raise TypeError
       end
      else
        Bool
    | Call (n, args) -> 
      let proto = List.assoc n ctx.funcs in
      let (ret, argt) = proto in
      let (good, types) = check_args argt args ctx in
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
    | StructMember (n, member) ->
      let struct_type = List.assoc n ctx.vars in
      let (name, struct_members) = match struct_type with
        | Struct (name, members) -> (name, members)
        | _ ->
          begin
            Printf.printf
              "Trying to access member %s of type %s which is not a struct.\n"
              member
              (type_to_string struct_type)
            ;
            raise TypeError
          end
      in
      let member_type =
        try List.assoc member struct_members
        with Not_found ->
          (* J'ai decide d'inclure cette erreur ici car meme si en soit ca n'est pas une
           * erreur de type, verfier ceci est obligatoire pour verifier les type.*)
          begin
            Printf.printf
              "Trying to access member %s which does not belong to the struct %s\n"
              member
              name
            ;
            raise TypeError
          end
      in
      member_type
    | Deref(expr) ->
      let deref_type = match (check_expr expr ctx) with
       | Ptr(typ) -> typ
       | typ ->
         begin
           Printf.printf
             "Trying to dereference an expression of type %s which is not a pointer type.\n"
              (type_to_string typ)
           ;
           raise TypeError
         end
      in
      deref_type
    | Address(expr) ->
      let expr_type = check_expr expr ctx in
      Ptr(expr_type)
(*
 *  Fonction qui permet de vérifier que tous les arguments donnés lors d'un appel
 *  de fonction sont du bon type.
 *  @param argst La liste des types des paramètres de la fonction
 *  @param args  La liste des expressions des arguments de l'appel de fonction.
 *  @return Un booléen vrai si tous les arguments sont de bon type, faux sinon
 *            et la liste des types des arguments, utile en cas d'erreur de typage.
 * *)
and check_args (argst: typ list) (args: expr list) (ctx: context): bool * typ list =
  let rec loop argst args res =
    match args with
    | [] -> res
    | hd::tl ->
      let (resb, resl) = res in
      let expectedt = List.hd argst in
      let actualt = check_expr hd ctx in
      if expectedt <> actualt then
        loop (List.tl argst) tl (false, resl@[actualt])
      else
        loop (List.tl argst) tl (resb, resl@[actualt])
  in
  loop argst args (true, [])

(*
 * Fonction permettant de vérifier qu'une liste d'initialisation a les bons type
 * pour sa structure.
 * @param members La liste des membres de la structure.
 * @param vals    La liste des expressions de la liste d'initialisation.
 * @return Void si la liste des expressions est bien typée.
 * @raise TypeError si la liste des expressions ext mal typée.
 *
 *  A noter que l'on ne vérifier pas si celle-ci est finie à la fin, on pourrait donc donner
 *  une liste d'initialisation plus grande que la structure, tant que l'on a les bons types pour
 *  tous les membres de structure.
 * *)
let rec check_struct_members (members: (string * typ) list) (vals:expr list) (ctx: context)=
  match members with
    | [] -> Void
    | (name, typ)::tl ->
      let expr_typ = check_expr (List.hd vals) ctx in
      if expr_typ = typ then
        check_struct_members tl (List.tl vals) ctx
      else
        begin
          Printf.printf
            "Trying to assign member %s with an expression of type %s, when type %s is expected.\n"
            name
            (type_to_string expr_typ)
            (type_to_string typ)
          ;
          raise TypeError
        end

(*
 * Fonction qui vérifier que les expressions donnés dans une instruction sont bien typés.
 * *)
let rec check_instr (instr: instr) (ctx: context): typ =
  match instr with
  | Putchar (_) -> Void
  | Set (n, e) ->
    let vart = List.assoc n (ctx.vars) in
    let exprt = check_expr e ctx in
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
    if (check_expr cond ctx) <> Bool then
      begin
        Printf.printf
          "The condition expect type bool instead of %s\n"
          (type_to_string (check_expr cond ctx));
        raise TypeError;
      end
    else
      begin
        let _ = check_seq then_seq ctx in
        let _ = check_seq else_seq ctx in
        Void
      end
  | While (cond, seq) ->
    if (check_expr cond ctx) <> Bool then
      begin
        Printf.printf
          "The condition expect type bool instead of %s\n"
          (type_to_string (check_expr cond ctx));
        raise TypeError;
      end
    else
      begin
        let _ = check_seq seq ctx in
        Void
      end
  | Return (e) ->
    let exprt = check_expr e ctx in
    if exprt <> ctx.current_ret_type then
      begin
        Printf.printf
          "Expected return type of %s instead of %s.\n"
          (type_to_string ctx.current_ret_type)
          (type_to_string exprt);
        raise TypeError
      end
    else
      ctx.current_ret_type
  | Expr (e) -> (check_expr e ctx)
  | SetStruct(n, vals) ->
    let struct_type = List.assoc n ctx.vars in
    let members = 
      match struct_type with
       | Struct(_ , members) -> members
       | _ ->
         begin
           Printf.printf
             "Trying to assign an initializer list varaible %s of type %s which is not a struct.\n"
             n
             (type_to_string struct_type)
           ;
           raise TypeError
         end
    in
    check_struct_members members vals ctx
  | SetStructMember(name, member, expr) ->
    let struct_type = List.assoc name ctx.vars in
    let struct_members =
      match struct_type with
        | Struct(_, members) -> members
        | _ ->
          begin
            Printf.printf
              "Trying to assign a value to the member %s of the variable %s which is not a struct.\n"
              member
              name
            ;
            raise TypeError
          end
    in
    let member_type = List.assoc member struct_members in
    let expr_type = check_expr expr ctx in
    if member_type = expr_type then
      Void
    else
      begin
        Printf.printf
          "Trying to assign an expression of type %s to the member %s of variable %s which is type %s.\n"
          (type_to_string expr_type)
          member
          name
          (type_to_string member_type)
        ;
        raise TypeError
      end
  | SetPtrVal(deref, expr) ->
    let var_name, member = match deref with
      | Deref(Get(name)) -> (name, None)
      | Deref(StructMember(name, member)) -> (name, Some member)
      | _ ->
        begin
          Printf.printf
            "Trying to set a value with derefence on a non assignable pointer expression.\n"
          ;
          raise TypeError
        end
    in
    let expr_type = check_expr expr ctx in
    match member with
      | None ->
        begin
          let var_type = List.assoc var_name ctx.vars in
          let ptr_type = match var_type with
            | Ptr(typ) -> typ
            | _ ->
              begin
                Printf.printf
                  "Trying to derefence variable %s which is not a pointer type.\n"
                  var_name
                ;
                raise TypeError
              end
          in
          if ptr_type = expr_type then
            Void
          else
            begin
              Printf.printf
                "Trying to set a value of type %s to pointer %s of type %s.\n"
                (type_to_string expr_type)
                var_name
                (type_to_string var_type)
              ;
              raise TypeError
            end
        end
      | Some(member_name) ->
        let struct_type = List.assoc var_name ctx.vars in
        let struct_members =
          match struct_type with
          | Struct(_, members) -> members
          | _ ->
            begin
              Printf.printf
                "Trying to assign a value to the member %s of the variable %s which is not a struct.\n"
                member_name
                var_name
              ;
              raise TypeError
            end
        in
        let member_type = List.assoc member_name struct_members in
        if expr_type <> member_type then
          begin
            Printf.printf
              "Trying to assign a value of type %s to %s.%s which is type %s.\n"
              (type_to_string expr_type)
              var_name
              member_name
              (type_to_string member_type)
            ;
            raise TypeError
          end
        else
          Void
and check_seq seq ctx =
  match seq with
  | [] -> Void
  | hd::tl ->
    let _ = check_instr hd ctx in
    check_seq tl ctx

(*
 * Fonction qui vérifie que le corps d'une fonction est entièrement bien typé.
 * @return Un context étendue de la fonction que l'on vien de vérifer
 * *)
let check_func (func: func_def) (ctx: context): context =
  let (_, paramst) = List.split func.params in
  (* On ajoute le prototype de la fonction que l'on évalue afin de pouvoir
   * évaluer des appels récursifs.
   * *)
  let proto = (func.name, (func.return, paramst)) in
  let ctx2 = {
    structs= ctx.structs;
    vars= ctx.vars@func.params@func.locals;
    funcs= ctx.funcs@[proto];
    current_ret_type = func.return;
  } in
  let _ = check_seq func.code ctx2 in
  {
    structs= ctx.structs;
    vars= ctx.vars;
    funcs= ctx2.funcs;
    current_ret_type = ctx.current_ret_type;
  }

(*
 * Fonction qui vérifie qu'une liste de fonction est bien typé.
 * @return Le contexte étendue de toutes les fonctions vérifiés.
 * *)
let rec check_funcs (funcs: func_def list) (ctx: context):context =
  match funcs with
  | [] -> ctx
  | hd::tl ->
    check_funcs tl (check_func hd ctx)

