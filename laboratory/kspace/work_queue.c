#ifdef __EXP_WORK_QUEUE__

void work_func(struct work_struct *work)
{
	printk("work_func\n");
}


DECLARE_WORK(work_struct, work_func);
DECLARE_DELAYED_WORK(d_work_struct, work_func);


void test_work_queue(void)
{
	schedule_work(&work_struct);
	schedule_delayed_work(&d_work_struct, 3*HZ);
}
#endif
