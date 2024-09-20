#ifdef __EXP_TIMER__
#include <linux/spinlock.h>
#include <linux/kthread.h>

static struct timer_list timer;
static spinlock_t sl;
static struct task_struct* ts;

static void timer_callback(struct timer_list * tl)
{
	mod_timer( &timer, jiffies + 1 * HZ );

	spin_lock(&sl);
	spin_unlock(&sl);
}

void unregister_timer(void)
{
	del_timer(&timer);
	kthread_stop(ts);
}

int cpu_occupy(void* arg)
{
	while(!kthread_should_stop())
	{
		prk("\n");
		msleep(1000);
	}
	return 0;
}

void exp_timer_test(void)
{
	int ret;

	timer_setup( &timer, timer_callback, 0 );
	spin_lock_init(&sl);

	if ( timer_pending( &timer ) )
	{
		prk( "timer_pending\n" );
	}
	ret = mod_timer( &timer, jiffies+HZ );

	register_early_exit(unregister_timer);

	ts = kthread_run(cpu_occupy, NULL, "%s", "cpu_occupy");
}
#endif
