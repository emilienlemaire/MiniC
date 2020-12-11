(* Minic Emilien Lemaire - Projet Cours de Compilation
 * Fichier: minic.ml
 * Le fichier principal contenant la fonction de depart du programme
 *
 * Notes:
 *   - J'ai utilisé l'API Incremental de Menhir, qui permet d'avoir des erreurs
 *      de syntaxe contextualiée très précises. Pour générer l'executable utilsez
 *      la commande:
 *        $ dune build
 *      Pour lancer le programme utilisez la commande:
 *        $ dune exec ./minic.exe [fichier MiniC]
 *  - Tous les type pour l'abre de syntaxe abstrait sont dans
 *      le fichier ast_types.ml
 *  - Le programme de verfication des types est dans le fichier
 *      minictypechecker.ml
 *  - Le fichier minicparser.messages contient tous les messages d'erreurs
 *      utilisés par menhir. Il est assez long mais vous pouvez y voir
 *      des exemples d'erreurs repérées grace à l'API Incremental de
 *      menhir
 * *)

open Minictypechecker
open Printer

module I = Minicparser.MenhirInterpreter

exception SyntaxError of ((int * int) option * string)

let get_pos (lexbuf: Lexing.lexbuf) =
  let curr_p = lexbuf.lex_curr_p in
  let line = curr_p.Lexing.pos_lnum in
  let col = curr_p.Lexing.pos_cnum - curr_p.Lexing.pos_bol in
  (line, col)

let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Error_messages.message (I.number state)) with
        | Not_found -> "Unknown syntax error."

let rec parse lexbuf (checkpoint : Ast_types.prog I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Miniclexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_pos lexbuf in
      let err = get_parse_error _env in
      raise (SyntaxError (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (SyntaxError (None, "Unknown syntax error."))

let main prog =
  let _ =
    try check_prog prog
    with TypeError msg ->
      Printf.eprintf "TypeError: %s\n" msg;
      exit 1
  in
  print_prog prog; exit 0

let _ =
  let cin = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel cin in
  let res =
    try Ok (parse lexbuf (Minicparser.Incremental.prog lexbuf.lex_curr_p))
    with SyntaxError (pos, err) ->
      match pos with
        | Some (line, col) -> Error (Printf.sprintf "Syntax error: %d:%d %s" line col err)
        | None -> Error (Printf.sprintf "Syntax error: %s" err)
  in
  match res with
    | Ok prog -> main prog
    | Error err -> Printf.eprintf "MiniC Error compiling: %s\n\t%s" Sys.argv.(1) err; exit 1
