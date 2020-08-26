test: parts_of_speech
	./parts_of_speech

parts_of_speech: main.o lexer.o
	g++ *.o -o parts_of_speech

%.o: %.cpp
	g++ $< -c -o $@

lexer.o: lexer.cc
	g++ lexer.cc -c -o lexer.o

lexer.cc: speech.l
	flex -o lexer.cc speech.l

clean:
	rm -f lexer.cc *.o parts_of_speech
