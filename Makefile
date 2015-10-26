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

travis-coveralls.sh:
	wget https://raw.githubusercontent.com/simonjbeaumont/ocaml-travis-coveralls/master/$@

coverage: travis-coveralls.sh
	COV_CONF="ocaml setup.ml -configure --enable-tests" bash travis-coveralls.sh

.PHONY: build doc test all install uninstall reinstall clean distclean configure coverage

config.mk:
	./configure
