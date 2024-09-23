#ifdef __EXP_MEMORY__

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/kobject.h>
#include <asm/pgtable.h>
#include <asm/page.h>


static void* gpa;
static struct kobject vm2phy;


/*****************************************************************
输入参数：
pid 接收待查询进程的PID
va 接收待查询的虚拟地址
*****************************************************************/
static void* find_pgd_init(pid_t pid, unsigned long va)
{
	struct task_struct *pcb_tmp = NULL;
	struct pid* pidptr;
	pgd_t *pgd_tmp = NULL;
	pud_t *pud_tmp = NULL;
	pmd_t *pmd_tmp = NULL;
	pte_t *pte_tmp = NULL;
	unsigned long pa;

	printk("-------------------------------------------\n");
	printk(KERN_INFO"PAGE_OFFSET = 0x%lx\n",PAGE_OFFSET);
	printk(KERN_INFO"PGDIR_SHIFT = %d\n",PGDIR_SHIFT);
	printk(KERN_INFO"PUD_SHIFT = %d\n",PUD_SHIFT);
	printk(KERN_INFO"PMD_SHIFT = %d\n",PMD_SHIFT);
	printk(KERN_INFO"PAGE_SHIFT = %d\n",PAGE_SHIFT);

	printk(KERN_INFO"PTRS_PER_PGD = %d\n",PTRS_PER_PGD);
	printk(KERN_INFO"PTRS_PER_PUD = %d\n",PTRS_PER_PUD);
	printk(KERN_INFO"PTRS_PER_PMD = %d\n",PTRS_PER_PMD);
	printk(KERN_INFO"PTRS_PER_PTE = %d\n",PTRS_PER_PTE);

	printk(KERN_INFO"PAGE_MASK = 0x%lx\n",PAGE_MASK);

	pidptr = find_vpid(pid);
	if (!pidptr)
	{
		printk(KERN_INFO"pidptr=%p\n",pidptr);
		return 0;
	}

	if(!(pcb_tmp = pid_task(pidptr, PIDTYPE_PID))) {
		printk(KERN_INFO"Can't find the task %d .\n",pid);
		return 0;
	}
	printk(KERN_INFO"pgd = 0x%p\n",pcb_tmp->mm->pgd);

	if(!find_vma(pcb_tmp->mm,va)){
		printk(KERN_INFO"virt_addr 0x%lx not available.\n",va);
		return 0;
	}

	pgd_tmp = pgd_offset(pcb_tmp->mm,va);
	printk(KERN_INFO"pgd_tmp = 0x%p\n",pgd_tmp);
	printk(KERN_INFO"pgd_val(*pgd_tmp) = 0x%lx\n",pgd_val(*pgd_tmp));
	if(pgd_none(*pgd_tmp)){
		printk(KERN_INFO"Not mapped in pgd.\n");
		return 0;
	}

	pud_tmp = pud_offset(pgd_tmp,va);
	printk(KERN_INFO"pud_tmp = 0x%p\n",pud_tmp);
	printk(KERN_INFO"pud_val(*pud_tmp) = 0x%lx\n",pud_val(*pud_tmp));
	if(pud_none(*pud_tmp)){
		printk(KERN_INFO"Not mapped in pud.\n");
		return 0;
	}

	pmd_tmp = pmd_offset(pud_tmp,va);
	printk(KERN_INFO"pmd_tmp = 0x%p\n",pmd_tmp);
	printk(KERN_INFO"pmd_val(*pmd_tmp) = 0x%lx\n",pmd_val(*pmd_tmp));
	if(pmd_none(*pmd_tmp)){
		printk(KERN_INFO"Not mapped in pmd.\n");
		return 0;
	}

	/*在这里，把原来的pte_offset_map()改成了pte_offset_kernel*/
	pte_tmp = pte_offset_kernel(pmd_tmp,va);

	printk(KERN_INFO"pte_tmp = 0x%p\n",pte_tmp);
	printk(KERN_INFO"pte_val(*pte_tmp) = 0x%lx\n",pte_val(*pte_tmp));
	if(pte_none(*pte_tmp)){
		printk(KERN_INFO"Not mapped in pte.\n");
		return 0;
	}
	if(!pte_present(*pte_tmp)){
		printk(KERN_INFO"pte not in RAM.\n");
		return 0;
	}
	pa = (pte_val(*pte_tmp) & PAGE_MASK) |(va & ~PAGE_MASK);
	printk(KERN_INFO"virt_addr 0x%lx in RAM is 0x%lx .\n", va, pa);
	
	va = *(unsigned long *)((char *)pa + PAGE_OFFSET);
	printk(KERN_INFO"content in 0x%lx is %02lx %02lx %02lx %02lx\n", pa,
		(va&0x000000FF),
		(va&0x0000FF00)>>8,
		(va&0x00FF0000)>>16,
		(va&0xFF000000)>>24);
	return (void*)pa;
}


void vm2phy_release(struct kobject *kobject)
{
	printk("eric_test: release .\n");
}

ssize_t vm2phy_show(struct kobject *kobject, struct attribute *attr,char *buf)
{
	sprintf(buf,"%p\n", gpa);
	return (ssize_t)strlen(buf);
}

void* map_phy_addr(void* addr)
{
	int pfn = (long)addr / PAGE_SIZE;
	int off = (long)addr % PAGE_SIZE;
	struct page* pp = pfn_to_page(pfn);
	return kmap( pp ) + off;
}

void unmap_phy_addr(void* addr)
{
	int pfn = (long)addr / PAGE_SIZE;
	int off = (long)addr % PAGE_SIZE;
	struct page* pp = pfn_to_page(pfn);
	kunmap( pp );
}

ssize_t vm2phy_store(struct kobject *kobject,struct attribute *attr,const char *buf, size_t count)
{
	char* p;
	int pid;
	unsigned long va;

	p = strsep((char**)&buf, " ");
	pid = simple_strtoul(p, NULL, 10);

	p = strsep((char**)&buf, " ");
	va = simple_strtoul(p, NULL, 16);

	gpa = find_pgd_init(pid, va);
	printk("pid=%d va=%p pa=%p\n", pid, (void*)va, gpa);

	p = map_phy_addr( gpa );
	memcpy( p, strsep((char**)&buf, " "), 5 );
	p[5] = 0;
	unmap_phy_addr(gpa);

	return count;
}

struct attribute vm2phy_attr = {
	.name = "vm2phy",
	.mode = S_IRWXUGO,
};

static struct attribute *def_attrs[] = {
	&vm2phy_attr,
	NULL,
};

struct sysfs_ops vm2phy_sysops =
{
	.show  = vm2phy_show,
	.store = vm2phy_store,
};

struct kobj_type ktype =
{
	.release       = vm2phy_release,
	.sysfs_ops     = &vm2phy_sysops,
	.default_attrs = def_attrs,
};

static void create_sysfs_file(void)
{
	struct module* mod = THIS_MODULE;
	struct kobject* kobj = &mod->mkobj.kobj;
	kobject_init(&vm2phy, &ktype);
	kobject_add(&vm2phy, kobj, "vm2phy");
}

void memort_test_exit(void)
{
	kobject_del(&vm2phy);
}

void memory_test(void)
{
	create_sysfs_file();
	register_early_exit(memort_test_exit);
}

#endif
