
(* This file was auto-generated based on "minicparser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "While should be inside a function body.\n"
    | 1 ->
        "Expecting identifier after type.\n"
    | 98 ->
        "Expecting  '(' or ';' or '=' after declaration.\n"
    | 4 ->
        "While should be inside a function body.\n"
    | 94 ->
        "Expecting  '(' or ';' or '=' after declaration.\n"
    | 10 ->
        "Function parameters should be a declaration (type ident).\n"
    | 89 ->
        "Parameters should be separated by commas.\n"
    | 90 ->
        "Function parameters should be a declaration (type ident).\n"
    | 12 ->
        "Missing '{' after function declaration.\n"
    | 14 ->
        "Missing '(' after 'while'.\n"
    | 15 ->
        "'while' condition should be an expression.\n"
    | 42 ->
        "Missing matching '('\n"
    | 43 ->
        "Missing '{' after 'while' condition.\n"
    | 84 ->
        "Missing left operand on binary operator.\n"
    | 44 ->
        "Missing left operand on binary operator.\n"
    | 71 ->
        "';' or '=' expected after local declaration\n"
    | 74 ->
        "Missing ';' at the end of a local declaration.\n"
    | 69 ->
        "Missing matching ')'.\n"
    | 26 ->
        "Missing right operand on binary operator.\n"
    | 86 ->
        "Missing left operand on binary operator.\n"
    | 31 ->
        "Missing right operand on binary operator.\n"
    | 32 ->
        "Missing ';' after expression.\n"
    | 33 ->
        "Missing right operand on binary operator.\n"
    | 34 ->
        "Missing ';' after expression.\n"
    | 13 ->
        "Missing left operand on a binary operator.\n"
    | 45 ->
        "'return' expects an expression.\n"
    | 46 ->
        "Missing ';' after return statement.\n"
    | 18 ->
        "Missing ';' after return statement.\n"
    | 48 ->
        "Missing '(' after puthcar call.\n"
    | 49 ->
        "'putchar' expects an expression as argument.\n"
    | 50 ->
        "Missing matching ')'.\n"
    | 51 ->
        "Missing ';' after function call.\n"
    | 17 ->
        "Expecting an expression inside parenthesis.\n"
    | 40 ->
        "Missing matching ')'.\n"
    | 53 ->
        "Missing '(' after if statement.\n"
    | 54 ->
        "if condition should be an expression.\n"
    | 55 ->
        "Missing matching ')'.\n"
    | 56 ->
        "Missing '{' after if condition.\n"
    | 76 ->
        "Missing left operand on a binary operator.\n"
    | 77 ->
        "Missing 'else' statement.\n"
    | 78 ->
        "Missing '{' after 'else' statement.\n"
    | 80 ->
        "Missing left operand on a binary operator.\n"
    | 79 ->
        "Missing left operand on a binary operator.\n"
    | 57 ->
        "Missing left operand on a binary operator.\n"
    | 58 ->
        "Identifier is expected to be followed by ';' or '(' or '=' or a binary operator.\n"
    | 19 ->
        "Missing matching ')'.\n"
    | 25 ->
        "Missing matching ')'.\n"
    | 35 ->
        "Function arguments are expected to be expressions.\n"
    | 59 ->
        "Expecting an expression after '='.\n"
    | 60 ->
        "Missing ';' at the end of a variable assignment.\n"
    | 6 ->
        "Unexpected statement outside of a function body.\n"
    | 9 ->
        "Missing '(' after function declaration.\n"
    | 72 ->
        "Global assignment expects an expression after '='.\n"
    | 95 ->
        "Missing ';' after global variable assignment.\n"
    | _ ->
        raise Not_found
