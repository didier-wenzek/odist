NAME = odist
DOC = odist.docdir/index.html
MODULES = odist_infix odist_fold odist_red odist_col odist_text odist_action odist_cluster odist_util odist_stream
CMI = $(addsuffix .cmi, $(MODULES))
TARGETS = odist.cma odist.cmxa odist.cmi odist.a $(CMI)
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB)

all:
	ocamlbuild -use-ocamlfind -cflags -safe-string $(TARGETS)

doc:
	ocamlbuild -use-ocamlfind $(DOC)

tests:
	ocamlbuild -use-ocamlfind -cflags -safe-string tests.native
	_build/tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

uninstall:
	ocamlfind remove $(NAME) 

clean:
	ocamlbuild -clean

.PHONY: all clean doc tests install uninstall
