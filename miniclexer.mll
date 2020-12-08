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
let cst   = ('-')? (digit)+

rule token = parse
  | ' '        { token lexbuf }
  | '\t'       { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
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

