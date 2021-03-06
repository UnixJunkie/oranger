.PHONY: build clean edit install uninstall reinstall test

build:
	dune build @install -j `getconf _NPROCESSORS_ONLN`

clean:
	dune clean

edit:
	emacs src/*.ml &

install: build
	dune install

test: build
	dune build src/test.exe
	_build/default/src/test.exe

uninstall:
	dune uninstall

reinstall: uninstall install
