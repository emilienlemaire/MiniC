{
  open Minicparser

  exception Eof
  
  let print_token t =
    match t with
      | TYPE(s) -> Printf.printf "TYPE(%s) " s
      | CONST(i) -> Printf.printf "CONST(%d) " i
      | IDENT(s) -> Printf.printf "IDENT(%s) " s
      | EQ -> Printf.printf "EQ "
      | PARO -> Printf.printf "PARO "
      | PARC -> Printf.printf "PARC "
      | COMMA -> Printf.printf "COMMA "
      | BRAO -> Printf.printf "BRAO "
      | BRAC -> Printf.printf "BRAC "
      | IF -> Printf.printf "IF "
      | ELSE -> Printf.printf "ELSE "
      | WHILE -> Printf.printf "WHILE "
      | RETURN -> Printf.printf "RETURN "
      | SEMI -> Printf.printf "SEMI "
      | LTH -> Printf.printf "LTH "
      | PLUS -> Printf.printf "PLUS "
      | TIMES -> Printf.printf "TIMES "
      | TRUE -> Printf.printf "TRUE "
      | FALSE -> Printf.printf "FALSE "
      | PUTCHAR -> Printf.printf "PUTCHAR "
      | EOF -> Printf.printf "EOF "

  let line = ref 1
  let col  = ref 0

  let space()   = incr col
  let tab()     = col := !col+2
  let newline() = col := 0; incr line

  let add_to_col n =
    col := !col + n
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha (alpha | digit | '_')*
let cst   = ('-')? (digit)+

rule token = parse
  | ' '        { space (); token lexbuf }
  | '\t'       { tab (); token lexbuf }
  | '\n'       { newline (); token lexbuf }
  | "int"      { add_to_col 3; TYPE "int"}
  | "bool"     { add_to_col 4; TYPE "bool" }
  | "void"     { add_to_col 4; TYPE "void" }
  | cst as n   { add_to_col (String.length n); CONST (int_of_string n) }
  | "true"     { add_to_col 4; TRUE }
  | "false"    { add_to_col 5; FALSE }
  | '='        { add_to_col 1; EQ }
  | '('        { add_to_col 1; PARO }
  | ')'        { add_to_col 1; PARC }
  | ','        { add_to_col 1; COMMA }
  | '{'        { add_to_col 1; BRAO }
  | '}'        { add_to_col 1; BRAC }
  | "if"       { add_to_col 2; IF }
  | "else"     { add_to_col 4; ELSE }
  | "while"    { add_to_col 5; WHILE }
  | "return"   { add_to_col 6; RETURN }
  | "putchar"  { add_to_col 6; PUTCHAR }
  | ';'        { add_to_col 1; SEMI }
  | '+'        { add_to_col 1; PLUS }
  | '*'        { add_to_col 1; TIMES }
  | '<'        { add_to_col 1; LTH }
  | ident as i { add_to_col (String.length i); IDENT (i) }
  | _ as c     { failwith ( Printf.sprintf
                "Unexpected character %d:%d '%c'" !line !col c ) }
  | eof        { EOF }

(*{*)
  (*let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))*)

  (*let rec loop () =*)
    (*let t = token lexbuf in*)
    (*if t <> EOF then*)
      (*begin*)
        (*print_token t;*)
        (*loop ()*)
      (*end*)
  
  (*let _ =*)
    (*loop ()*)
(*}*)
