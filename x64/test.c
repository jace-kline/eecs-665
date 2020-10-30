#include <stdio.h>

int main() {
	int b = 7;
	int * a = &b;
	printf("%p", a);
	return 0;
}
