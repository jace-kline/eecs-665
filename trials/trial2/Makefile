HS_FILES := $(wildcard *.hs)

.PHONY: clean test

all: build

test: build parser grammar.encoded
	./parser test.tokens

run: dragonsdt
	./dragonsdt input.ag

build: dragonsdt

dragonsdt: $(HS_FILES)
	ghc -o dragonsdt BuildGrammar.hs

parser: $(HS_FILES)
	ghc -o parser Main.hs

grammar.encoded: run

clean:
	rm -rf dragonsdt parser grammar.encoded *.hi *.o