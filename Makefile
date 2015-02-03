NAME = odist
DOC = odist.docdir/index.html
MODULES = infix fold red col text action cluster util
CMI = $(addsuffix .cmi, $(MODULES))
TARGETS = odist.cma odist.cmxa odist.cmi odist.a $(CMI)
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB)

all:
	ocamlbuild -use-ocamlfind $(TARGETS)

doc:
	ocamlbuild $(DOC)

tests:
	ocamlbuild -use-ocamlfind -lflags -cclib,-lzmq tests.native --
	_build/tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

uninstall:
	ocamlfind remove $(NAME) 

clean:
	ocamlbuild -clean

.PHONY: all clean doc tests install uninstall
