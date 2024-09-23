#include <sys/mman.h>  
#include <sys/stat.h>  
#include <fcntl.h>  
#include <stdio.h>  
#include <stdlib.h>  
#include <unistd.h>  
#include <error.h> 

int main()
{
	int fd = 0;
	unsigned char* start_fp = NULL;
	
	printf("Please run:\ncat /proc/%d/maps\n", getpid());

	if( (fd = open("/dev/tcd",O_RDONLY) ) < 0 )
	{
		printf("open /dev/tcd failed.\n");
		exit(0);
	}

	if( ( start_fp = mmap(NULL, 1024, PROT_READ,MAP_SHARED, fd, 0 )) == (void *)-1)
	{
		printf("mmap error !\n");
		exit(0);
	}

	int i = 0;
	for (; i<6; i++)
		printf("%02X ", start_fp[i]);
	printf("\n");

	sleep(10);

	munmap(start_fp, 1024);
	close(fd);

	return 0;
}
