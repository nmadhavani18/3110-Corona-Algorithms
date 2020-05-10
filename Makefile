MODULES=engine authors command run simple_threshold mean_reversion
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=run.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -r

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh && bash checktypes.sh
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip algorithms.zip *.ml* *.json _tags Makefile
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,lambdasoup,core,lwt,cohttp,cohttp-lwt-unix\
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,lambdasoup,core,lwt,cohttp,cohttp-lwt-unix\
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private algorithms.zip
