LFLAGS := -g

all: clean test

.PHONY: clean test

calc: calc.o lexer.o parser.o
	g++ -o calc calc.o lexer.o parser.o

%.o: %.cpp
	g++ $(LFLAGS) -c $< -o $@
				
lexer.o: lexer.cc
	g++ $(LFLAGS) -c lexer.yy.cc -o lexer.o

parser.o: parser.cc
	g++ $(LFLAGS) -c parser.cc -o parser.o

lexer.cc: grammar.hh lexer.l
	flex --outfile lexer.cc lexer.l

grammar.hh: parser.yy
	bison --defines=grammar.hh -d -v parser.yy

parser.cc: parser.yy
	bison --defines=grammar.hh -d -v parser.yy

clean:
	rm -rf calc *.o *.cc *.hh
