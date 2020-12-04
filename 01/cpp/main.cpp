#include <iostream>
#include <fstream>
extern "C" char* foo(void);

int main() {
	char* res =  foo();
	std::cout<<"Foo returns "<< res <<"\n";
	return 0;
}