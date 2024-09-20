#include <stdio.h>  
#include <sys/time.h>  
#include <linux/input.h>  
#include <stdlib.h>  

void usage(char *str)  
{  
	fprintf(stderr, "<usage> %s /dev/input/eventX\n", str);  
	exit(1);  
} 

int main(int argc,char **argv)  
{  
	FILE *fp;  
	struct input_event ie;  
	if (argc != 2)  
		usage(argv[0]);  
	fp = fopen(argv[1], "r");  
	if (fp == NULL) {  
		perror(argv[1]);  
		exit(1);  
	}  
	while (1) {  
		fread((void *)&ie, sizeof(ie), 1, fp);  
		if (ferror(fp)) {  
			perror("fread");  
			exit(1);  
		}  
		printf("[timeval:sec:%d,usec:%d,type:%d,code:%d,value:%d]\n",  
		ie.time.tv_sec, ie.time.tv_usec,  
		ie.type, ie.code, ie.value);  
		if(ie.type==0)
			printf("\n");
	}  
	return 0;  
}  
