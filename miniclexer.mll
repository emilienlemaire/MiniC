{
  open Minicparser

  exception Eof
  
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
let cst   = (digit)+

rule token = parse
  | ' '        { token lexbuf }
  | '\t'       { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | "//"       { one_line_comment lexbuf }
  | "/*"       { multiple_lines_comment lexbuf }
  | "int"      { TYPE "int"}
  | "bool"     { TYPE "bool" }
  | "void"     { TYPE "void" }
  | cst as n   { CONST (int_of_string n) }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "=="       { EQEQ }
  | "!="       { NEQ }
  | '='        { EQ }
  | '('        { PARO }
  | ')'        { PARC }
  | ','        { COMMA }
  | '{'        { BRAO }
  | '}'        { BRAC }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "return"   { RETURN }
  | "putchar"  { PUTCHAR }
  | "struct"   { STRUCT }
  | "->"       { ARROW }
  | ';'        { SEMI }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '/'        { BY }
  | '*'        { TIMES }
  | '%'        { MOD }
  | "<="       { LEQ }
  | ">="       { GEQ }
  | '<'        { LTH }
  | '>'        { GTH }
  | "&&"       { AND }
  | "||"       { OR }
  | '.'        { DOT }
  | '!'        { NOT }
  | '&'        { ADDRESS }
  | ident as i { IDENT (i) }
  | _ as c     { failwith ( Printf.sprintf
                "Unexpected character %d:%d '%c'" !line !col c ) }
  | eof        { EOF }

and one_line_comment = parse
  | '\n' { token lexbuf }
  | _ { one_line_comment lexbuf }

and multiple_lines_comment = parse
  | "*/" { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; multiple_lines_comment lexbuf }
  | _ { multiple_lines_comment lexbuf }
