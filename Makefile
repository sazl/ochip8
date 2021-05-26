all: main.exe

main.exe: bin/main.ml
	dune build bin/$@

test: tests/*
	dune runtest

run: main.exe
	dune exec bin/main.exe

.PHONY: clean
clean:
	dune clean