all: aps.byte test.byte

build/:
	mkdir build

build/lexer.ml: build/ src/lexer.mll
	ocamllex src/lexer.mll
	mv src/lexer.ml build/

build/parser.mli build/parser.ml: build/ src/parser.mly
	menhir src/parser.mly
	mv src/parser.ml src/parser.mli build/

aps.byte: build/ src/*.ml src/typer.pl build/parser.ml build/lexer.ml
	cp src/* build
	cd build && ocamlc -o aps.byte \
			ast.ml parser.mli parser.ml lexer.ml typer.ml eval.ml aps.ml
	ln -sf build/aps.byte aps.byte

test.byte: build/ src/*.ml src/typer.pl build/parser.ml build/lexer.ml test.ml
	cp test.ml build
	cd build && ocamlc -o test.byte \
			unix.cma \
			test.ml
	ln -sf build/test.byte test.byte

clean:
	rm -rf build/ aps.byte

.PHONY: all clean
