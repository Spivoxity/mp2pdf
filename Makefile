# -*-Makefile-*-

all: mp2pdf

mp2pdf: mp2parse.o mp2scan.o
	gcc -g -o $@ $^

mp2parse.c mp2parse.h: mp2parse.y
	bison -v -d -o mp2parse.c $<

mp2scan.c: mp2scan.l
	flex -o $@ $< 

mp2scan.o mp2parse.o: mp2parse.h

CC = gcc
CFLAGS = -g

########

mp2pdf-old: parser.cmo scanner.cmo
	ocamlc -g -o $@ $^

%.cmo: %.ml
	ocamlc -c -g $<

%.cmi: %.mli
	ocamlc -c -g $<

parser.mli parser.ml: parser.mly
	ocamlyacc -v $<

scanner.ml: scanner.mll
	ocamllex $<

parser.cmo scanner.cmo: parser.cmi

########

clean: force
	rm -f mp2pdf *.o mp2parse.[cho] mp2parse.output mp2scan.[co]

force:
