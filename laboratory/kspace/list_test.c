#ifdef __EXP_LIST__




LIST_HEAD(test_list);

struct test_node
{
	struct list_head node;
	int i;
};

void list_test(void)
{
	int i = 0;
	struct test_node* it;
	struct test_node* safe;

	for(;i<10;i++)
	{
		struct test_node* p = kmalloc( sizeof(struct test_node), GFP_KERNEL);
		p->i = i;
		list_add(&p->node, &test_list);
	}
	
	list_for_each_entry_safe(it, safe, &test_list, node)
	{
		printk("i=%d\n", it->i);
		list_del(&it->node);
		kfree(it);
	}
}

#endif
