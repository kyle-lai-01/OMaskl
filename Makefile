UNITS=ast eval inputoutput command translator interp
MLS=$(UNITS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
RUN=inputoutput.byte
OCAMLBUILD=ocamlbuild -r -use-ocamlfind -menhir "menhir --table --explain"
PKGS=oUnit,str,ANSITerminal

build:
	$(OCAMLBUILD) $(OBJECTS)

default: all

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

run:
	$(OCAMLBUILD) $(RUN) && ./$(RUN)


check:
	bash checkenv.sh 

finalcheck: check
	bash finalcheck.sh

zip:
	zip omaskl.zip *.ml* *.txt* *.pdf* _tags Makefile  

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report omaskl.zip bisect*.out
