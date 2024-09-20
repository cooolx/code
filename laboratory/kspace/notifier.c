#ifdef __EXP_NOTIFIER__

#include <linux/notifier.h>
//struct blocking_notifier_head bnh;

BLOCKING_NOTIFIER_HEAD(nbh);

int notify_func(struct notifier_block* nb, unsigned long val, void* p)
{
	prk("val=%lu p=%p\n", val, p);
	return 0;
}

struct notifier_block nb = {
	.notifier_call = notify_func,
};

void notify_test(void)
{
	blocking_notifier_chain_register(&nbh, &nb);
	blocking_notifier_call_chain(&nbh, 1, NULL);
	blocking_notifier_chain_unregister(&nbh, &nb);
	blocking_notifier_call_chain(&nbh, 2, NULL);
}

#endif
