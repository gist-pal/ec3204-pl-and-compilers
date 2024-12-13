all: run

run: lexer.cmo parser.cmo s.cmo t.cmo translator.cmo optimizer.cmo main.cmo
		ocamlfind ocamlc -thread -package batteries -linkpkg -o run lexer.cmo parser.cmo s.cmo t.cmo translator.cmo optimizer.cmo main.cmo

s.cmo : s.ml
		ocamlfind ocamlc -thread -package batteries -linkpkg -c s.ml

t.cmo : t.ml
		ocamlfind ocamlc -thread -package batteries -linkpkg -c t.ml

translator.cmo : s.cmo t.cmo translator.ml
		ocamlc -c translator.ml

optimizer.cmo : t.cmo
		ocamlfind ocamlc -thread -package batteries -linkpkg -c optimizer.ml

parser.ml: parser.mly s.cmo
		ocamlyacc parser.mly

parser.mli: parser.mly
		ocamlyacc parser.mly

parser.cmi: parser.mli
		ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
		ocamlc -c parser.ml

main.cmo : s.cmo t.cmo translator.cmo optimizer.cmo main.ml
		ocamlc -c main.ml

lexer.cmo: lexer.ml
		ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
		ocamllex lexer.mll

clean:
		rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
