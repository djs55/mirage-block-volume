include config.mk

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	ocamlfind remove lvm
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

release:
	# Remove our dependencies on oasis and bisect
	sed -i -r s'/, bisect//g' _oasis
	sed -i -r s'/\"bisect\"//g' opam
	sed -i -r s'/\"oasis\"//g' opam
	# Remove our aversion to OASIS autogen
	sed -i -r s'/setup.ml//g' .gitignore
	sed -i -r s'/myocamlbuild.ml//g' .gitignore
	sed -i -r s'/_tags//g' .gitignore
	sed -i -r s'/\*.mllib//g' .gitignore
	sed -i -r s'/\*.mldylib//g' .gitignore
	sed -i -r s'/\*.mlpack//g' .gitignore
	sed -i -r s'/META//g' .gitignore
	oasis setup

coverage:
	rm -f _build/*.out
	BISECT_FILE=_build/coverage ./vg_test.native
	(cd _build; bisect-report co*.out -summary-only -html /vagrant/report/)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

config.mk:
	./configure
