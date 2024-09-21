#ifdef __EXP_TASKLET__

#include <linux/interrupt.h>
#include <linux/sched.h>

struct tasklet_struct tl;

void tasklet_fun(unsigned long p)
{
	// can schedule in softirq context, but will get a warning
	set_current_state(TASK_INTERRUPTIBLE);
	schedule_timeout(3 * HZ);

	prk("\n");
}


void test_tasklet(void)
{
	tasklet_init(&tl, tasklet_fun, 0);
	tasklet_schedule(&tl);
}

#endif
