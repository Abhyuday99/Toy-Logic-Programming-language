all:
	ocamlc -c ass6_backend.ml
	ocamllex lexer_final.mll       # generates lexer.ml
	ocamlyacc parser_ass6.mly     # generates parser.ml and parser.mli
	ocamlc -c parser_ass6.mli
	ocamlc -c lexer_final.ml
	ocamlc -c parser_ass6.ml
	ocamlc -c ass6_main.ml
	ocamlc -o assignment6  str.cma ass6_backend.cmo lexer_final.cmo parser_ass6.cmo ass6_main.cmo
