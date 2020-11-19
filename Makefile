all: minic

minic: miniclexer.cmo minicparser.cmo minic.cmo
	ocamlc -o minic $?

minic.cmo:
	ocamlc -c minic.ml

miniclexer.cmo: miniclexer.ml minicparser.cmo
	ocamlc -c miniclexer.ml

miniclexer.ml:
	ocamllex miniclexer.mll

minicparser.cmo: minicparser.mli
	ocamlc -c minicparser.mli
	ocamlc -c minicparser.ml

minicparser.mli: ast_types.cmo
	menhir -v minicparser.mly

ast_types.cmo:
	ocamlc -c ast_types.ml

clean:
	rm *.{cmo,cmi,automaton,conflicts} miniclexer.ml minicparser.{ml,mli}
