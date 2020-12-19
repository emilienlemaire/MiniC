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

open Core
open MinicTypeChecker
open MinicInterpreter

module I = MinicParser.MenhirInterpreter

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
        | Not_found_s msg -> "Unknown syntax error."

let rec parse lexbuf (checkpoint : MinicAstTypes.prog I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = MinicLexer.token lexbuf in
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

let run_interpreter prog =
  let () =
    try check_prog prog
    with TypeError msg ->
      Printf.eprintf "TypeError: %s\n" msg;
      exit 1
  in
  interpret prog

let run_printer prog =
  let () =
    try check_prog prog
    with TypeError msg -> 
      Printf.eprintf "TypeError: %s\n" msg;
      exit 1
  in
  MinicPrinter.print_prog prog

let main filename =
  let cin = In_channel.create filename in
  let lexbuf = Lexing.from_channel cin in
  let res =
    try Ok (parse lexbuf (MinicParser.Incremental.prog lexbuf.lex_curr_p))
    with SyntaxError (pos, err) ->
      match pos with
        | Some (line, col) -> Error (Printf.sprintf "Syntax error: %d:%d %s" line col err)
        | None -> Error (Printf.sprintf "Syntax error: %s" err)
  in
  In_channel.close cin;
  match res with
    | Ok prog -> prog
    | Error err -> Printf.eprintf "MiniC Error compiling: %s\n\t%s" filename err; exit 1

let command =
  Core.Command.basic
    ~summary: "Interprete a MiniC file."
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open
        display = flag "-display" no_arg ~doc:" displays the code from the generated AST."
        and filename =
          anon (maybe_with_default "-" ("filename" %: Filename.arg_type))
      in
      fun () ->
        let prog = main filename in
        if display then
          run_printer prog
        else
          run_interpreter prog
    )

let () = Command.run command
