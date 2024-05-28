// week1.cpp : Defines the entry point for the application.
//

#include "week1.h"

#define usingNamespaces 0

#if usingNamespaces
using namespace std;
#endif

int main()
{
	

	#if usingNamespaces
	#pragma message("Usingnamespaces")
		cout << "Hello world!" << endl;

	#else
	#pragma message("Not Usingnamespaces")
		std::cout << "Hello World!" << std::endl;

	#endif
	return 0;
}
