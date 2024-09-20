#ifndef __KSPACE_H_
#define __KSPACE_H_

#define prk(fmt, ...) \
	printk(fmt, ##__VA_ARGS__);

#define prfl(fmt, ...) \
	prk("%d %s:%d "fmt, smp_processor_id(), \
	__FILE__, __LINE__, ##__VA_ARGS__)

#define prfn() \
	prfl("%s\n", __func__)


typedef void (*fun_exit)(void);
typedef struct early_exit
{
	struct list_head node;
	fun_exit  exit;
} early_exit_t;
LIST_HEAD(exit_head);


int register_early_exit(fun_exit func)
{
	struct early_exit* exit;

	if (!func)
		return -1;

	exit       = kzalloc(sizeof(struct early_exit), GFP_KERNEL);
	exit->exit = func;
	list_add(&exit->node, &exit_head);
	return 0;
}

#endif