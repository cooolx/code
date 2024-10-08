#include <linux/module.h>
#include <linux/init.h>
#include <linux/kobject.h>
#include <linux/fs.h>
#include <linux/list.h>
#include <linux/slab.h>
#include <linux/delay.h>
#include <asm/uaccess.h>
#include <linux/platform_device.h>
#include <linux/slab.h>
#include <linux/vmalloc.h>

// keep this order
#include "kspace.h"
#include "switch.h"

#include "timer.c"
#include "dev_mode.c"
#include "dev_class.c"
#include "kernel_file_access.c"
#include "tasklet.c"
#include "work_queue.c"
#include "lock.c"
#include "sched.c"
#include "kernel_exec.c"
#include "list_test.c"
#include "tasks.c"
#include "netlink.c"
#include "memory.c"
#include "mem_eat.c"
#include "input_test.c"
#include "input_fake.c"
#include "notifier.c"
#include "chdev.c"
#include "proc.c"
#include "mm_page.c"


void exp_test(void)
{
#ifdef __EXP_PROC__
	exp_create_proc();
#endif

#ifdef __EXP_CHAR_DEV__
	exp_char_dev();
#endif

#ifdef __EXP_TIMER__
	exp_timer_test();
#endif

#ifdef __EXP_DEV_MODEL__
	exp_dev_model();
#endif

#ifdef __EXP_DEV_CLASS__
	exp_dev_class();
#endif

#ifdef __EXP_KERNEL_FILE_ACCESS__
	kernel_file_access();
#endif

#ifdef __EXP_TASKLET__
	test_tasklet();
#endif

#ifdef __EXP_WORK_QUEUE__
	test_work_queue();
#endif

#ifdef __EXP_LOCK__
	exp_lock_init(exp_device);
#endif


#ifdef TEST_SCHED
	printk("before schedule\n");
	set_current_state(TASK_INTERRUPTIBLE);
	schedule_timeout(1000);
	printk("end schedule\n");
#endif

#ifdef __EXP_KERNEL_EXEC__
	test_kernel_exec();
#endif

#ifdef __EXP_LIST__
	list_test();
#endif

#ifdef __EXP_TASKS__
	print_tasks();
#endif

#ifdef __EXP_NETLINK__
	netlink_test();
#endif

#ifdef __EXP_MEMORY__
	memory_test();
#endif

#ifdef __EXP_MEMORY_EAT__
	exp_mem_eat_init(exp_device);
#endif

#ifdef __EXP_INPUT__
	input_test();
#endif

#ifdef __EXP_INPUT_FAKE__
	fake_init();
#endif

#ifdef __EXP_NOTIFIER__
	notify_test();
#endif

#ifdef __EXP_MM_PAGE__
	test_mm_page();
#endif
}

int ks_exp_init(void)
{
	int ret;

	prfn();
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

	exp_test();
	return 0;
}

void ks_exp_exit(void)
{
	struct early_exit* it;
	struct early_exit* safe;

	prfn();
	list_for_each_entry_safe(it, safe, &exit_head, node)
	{
		it->exit();
		prfl("Unregister eraly exit function:%pS\n", it->exit);
		list_del(&it->node);
		kfree(it);
	}
}


module_init( ks_exp_init );
module_exit( ks_exp_exit );

MODULE_AUTHOR("lzd");
MODULE_LICENSE("Dual BSD/GPL");