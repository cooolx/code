#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/fs.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/version.h>
#include <linux/platform_device.h>
#include <linux/slab.h>
#include <linux/vmalloc.h>

#include "exp_common.c"
#include "exp_lock.c"
#include "exp_mem.c"
#include "exp_sched.c"

static struct platform_device *exp_device;

static int __init exp_init(void)
{
	int ret;
	
	pr_info("exp_init\n");
	exp_device = platform_device_alloc("exp_device", 0);
	if (!exp_device) {
		pr_err("platform_device_alloc failed.\n");
		return -ENOMEM;
	}
	ret = platform_device_add(exp_device);
	if (ret < 0) {
		pr_err("platform_device_add failed.\n");
		return -1;
	}
	
	exp_lock_init(exp_device);
	exp_mem_init(exp_device);
	
	return 0;
}

static void __exit exp_exit(void)
{
}

module_init(exp_init);
module_exit(exp_exit);

MODULE_LICENSE("GPL");