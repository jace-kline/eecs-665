#include "stdio.h"
#include "stdlib.h"

void printBool(int8_t b){
	if (b == 0){ fprintf(stdout, "0"); }
	else { fprintf(stdout, "1"); }
	fflush(stdout);
}

void printChar(char c){
	fprintf(stdout, "%c", c);
	fflush(stdout);
}

void printInt(long int num){
	fprintf(stdout, "%ld", num);
	fflush(stdout);
}

void printString(const char * str){
	fprintf(stdout, "%s", str);
	fflush(stdout);
}

int8_t getBool(){
	char c;
	scanf("%c", &c);
	getchar(); // Consume trailing newline
	if (c == 0){
		return 0;
	} else {
		return 1;
	}
}

long int getInt(){
	char buffer[32];
	fgets(buffer, 32, stdin);
	long int res = atol(buffer);
	return res;
}

char getChar(){
	char c;
	scanf("%c", &c);
	getchar(); // Consume trailing newline
	return c;
}
