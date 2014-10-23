NAME = odist
DOC = odist.docdir/index.html
TARGETS = odist.cma odist.cmxa odist.cmi odist.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB)

all:
	ocamlbuild $(TARGETS)

doc:
	ocamlbuild $(DOC)

tests:
	ocamlbuild -libs unix,nums,ozmq -lflags -cclib,-lzmq tests.native --
	_build/tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean

.PHONY: all clean doc tests
