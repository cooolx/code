#ifdef __EXP_KERNEL_EXEC__

static int errno;
#include <linux/umh.h>

void test_kernel_exec(void)
{
	int ret = 0;
	static char *argv[] = { "/usr/bin/ftp", NULL };
	static char *envp[] = { "HOME=/", "TERM=linux", "PATH=/sbin:/usr/sbin:/bin:/usr/bin", NULL };
	
	printk("before execve\n");
	ret = call_usermodehelper("/usr/bin/ftp", argv, envp, UMH_WAIT_EXEC);
	printk("after execve ret=%d errno=%d\n", ret, errno);
}

#endif
