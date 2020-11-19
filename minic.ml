let _ = 
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    while true do
       Minicparser.prog Miniclexer.token lexbuf
    done
  with Miniclexer.Eof ->
    exit 0
