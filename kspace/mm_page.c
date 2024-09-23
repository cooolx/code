#ifdef __EXP_MM_PAGE__

#include <linux/gfp.h>

void test_mm_page(void)
{
	struct page* pg;
	void* addr;
	pg = alloc_pages(__GFP_HIGHMEM, 0);
	printk("alloc_pages return %p\n", pg);
	addr = kmap(pg);
	printk("kmap return %p\n", addr);
	kunmap(pg);
	__free_pages(pg, 0);


	addr = (void*)__get_free_pages(__GFP_HIGHMEM, 0);
	printk("get free page return %p\n", addr);
	free_pages((unsigned long)addr, 0);
}

#endif
