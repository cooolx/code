#ifdef __EXP_TASKS__

#include<linux/init.h>
#include<linux/module.h>
#include<linux/sched.h>
#include<linux/sem.h>
#include<linux/list.h>

struct mm_struct* cur;
struct mm_struct* new;


int print_tasks(void)
{
	struct task_struct *task,*pos;
	int count = 0;

	task = &init_task;
	list_for_each_entry(pos, &task->tasks, tasks)
	{
		count++;
		printk(KERN_ALERT"%04d comm:%s\n", pos->pid, pos->comm);

if (!strcmp(pos->comm, "a.out"))
	printk("~~~~~~~~~~~~~~~~~~~~~~~\n");

#if 0
		if (pos->active_mm)
			printk(KERN_ALERT"pgd:%p ldt:%p\n", pos->active_mm->pgd, pos->active_mm->context.ldt);
		if (pos->mm)
			printk(KERN_ALERT"pgd:%p ldt:%p\n", pos->mm->pgd, pos->mm->context.ldt);
#endif
	}

	printk(KERN_ALERT"the number of process is:%d\n", count);
	return 0;
}

#endif

