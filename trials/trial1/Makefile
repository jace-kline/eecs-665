HS_FILES := $(wildcard *.hs)

.PHONY: clean test

all: build

test: build lexer spec.encoded
	./lexer test.txt > test.tokens

run: dragonlex
	./dragonlex HolyC.spec

build: dragonlex

dragonlex: $(HS_FILES)
	ghc -o dragonlex SpecMain.hs

lexer: $(HS_FILES)
	ghc -o lexer LexMain.hs

spec.encoded: run

clean:
	rm -rf dragonlex lexer test.tokens spec.encoded *.hi *.o