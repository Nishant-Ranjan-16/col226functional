all:
	ocamlc -o krivine krivine.ml
	ocamlc -o k_ext k_ext.ml
	ocamlc -o secd_ext secd_ext.ml
	ocamlc -o secd secd.ml
