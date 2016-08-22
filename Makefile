OCB=ocamlbuild -tag thread,debug -use-ocamlfind -pkgs bistro.bioinfo,bistro.utils,ppx_bistro

all:
	$(OCB) src/insertion_ET.byte

clean:
	ocamlbuild -clean
