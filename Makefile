DEBUG=-DDEBUG
PP=-pp "cpp -w $(DEBUG)" #"camlp4o pa_macro.cmo $(DEBUG)"
OCAMLOPT=ocamlopt $(PP)
OCAMLC=ocamlc $(PP)
OCAMLDOC=ocamldoc $(PP)
CFLAGS=-Wall

SRC=pijul.cmx interaction.cmx commands.cmx main.cmx
pijul:$(SRC) mdb.cmxa
	ocamlfind ocamlopt -package cryptokit -o $@ -cclib -L. mdb.cmxa str.cmxa -linkpkg $(SRC)

pijul.cmx:mdb.cmxa pijul.cmi
pijul.cmi:mdb.cmxa
commands.cmx:pijul.cmx interaction.cmx
interaction.cmx:pijul.cmx
main.cmx:commands.cmx pijul.cmx
test.cmx:pijul.cmx

%.cmx:%.ml
	ocamlfind $(OCAMLOPT) -package cryptokit -c -w A -o $@ $<
%.cmi:%.mli
	$(OCAMLC) -c -w A -o $@ $<

mdb.cmxa:mdb_constants.ml mdb.ml lmdb_stubs.o
	ocamlmklib -o mdb lmdb_stubs.o mdb_constants.ml mdb.ml -llmdb -linkall


mdb_constants.ml:make_stubs
	bash make_stubs

lmdb_stubs.o:lmdb_stubs.c
	cc -fPIC -Wall -c -o $@ $<

.PHONY:doc remotedoc
doc:
	ocamldoc -html -d doc pijul.mli

remotedoc:doc
	rsync -r doc gitit@ovh:pijul/static/


clean:
	rm -f *~ *.cm[oxai] *.cmxa *.o *.so *.a
	rm -Rf doc

TESTS=tests/basic.tested tests/linedel.tested tests/unrecord.tested tests/rollback.tested

shell-tests: pijul $(TESTS)

%.tested:%.sh
	pijul=`pwd`/pijul bash $<
