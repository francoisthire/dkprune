.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

dkprune:
	@ln -s _build/install/default/bin/dkprune dkprune || true

.PHONY: debug
debug:


.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
