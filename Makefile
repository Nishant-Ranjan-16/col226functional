all:
	ocamlc -o krivine krivine.ml
	ocamlc -c secd.ml
	ocamlc -o secd krivine.cmo secd.cmo
