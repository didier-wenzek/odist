NAME = odist
DOC = odist.docdir/index.html
TARGETS = odist.cma odist.cmxa odist.cmi odist.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) sequence.mli

all:
	ocamlbuild $(TARGETS)

doc:
	ocamlbuild $(DOC)

tests:
	ocamlbuild -libs nums tests.native --

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean

.PHONY: all clean doc tests
