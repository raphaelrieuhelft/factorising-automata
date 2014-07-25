OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix \
		-Is factor

TARGET=native

all: fact0 fact_fixedlengths fact_sigs

fact0:
	$(OCAMLBUILD) fact0.$(TARGET)

fact_fixedlengths:
	$(OCAMLBUILD) fact_fixedlengths.$(TARGET)

fact_sigs:
	$(OCAMLBUILD) fact_sigs.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean