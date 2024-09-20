#include <stdio.h>

int main()
{
	char* p = "hello";
	
	while (1)
	{
		printf("%p %s\n", p, p);
		sleep(1);
	}

	return 0;
}
