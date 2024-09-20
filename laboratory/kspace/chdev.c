#ifdef __EXP_CHAR_DEV__

#include <linux/types.h>
#include <asm/uaccess.h>
#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/mm.h>
#include <linux/pagemap.h>


static dev_t exp_dt;
static struct cdev* exp_cdev;

int exp_cdev_open(struct inode * node, struct file * f)
{
	prfn();
	return 0;
}

int exp_cdev_release(struct inode * node, struct file * f)
{
	prfn();
	return 0;
}

//static int exp_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf) 
static vm_fault_t exp_vma_fault(struct vm_fault *vmf)
{
	struct vm_area_struct *vma = vmf->vma;
	unsigned long pfn = vma->vm_pgoff;
	struct page *page;
	void* pkm;
	
	printk(KERN_INFO  "PAGE FAULT\n");
	page = pfn_to_page(pfn);
	if(!page) {
		prfl("\n");
		return VM_FAULT_SIGBUS;
	}
	pkm = kmap(page);
	memset(pkm, 6, 16);
	kunmap(page);

	get_page(page);
	vmf->page = page;
	return 0;
}

static struct vm_operations_struct chr_nopage_vm_ops = {
    fault:  exp_vma_fault,
};

int exp_cdev_mmap(struct file * f, struct vm_area_struct * vma)
{
	prfn();
#if 0
	void* pkm = kmalloc(4096, GFP_KERNEL);
	unsigned int fn = virt_to_phys((void*)((unsigned long)pkm)) >> PAGE_SHIFT;
	memset(pkm, 6, 16);

	int ret = remap_pfn_range(vma, vma->vm_start, fn,
		vma->vm_end-vma->vm_start, PAGE_SHARED);
	prfl("fn=%d pkm=%p ret=%d\n", fn, pkm, ret);

	//vma->vm_ops = &chr_remap_vm_ops;
#else
	vma->vm_ops = &chr_nopage_vm_ops;
#endif
	return 0;
}

ssize_t exp_cdev_read(struct file * f, char __user * buff, size_t s, loff_t * o)
{
	prfn();
	return 0;
}

struct file_operations exp_chr_fops = {
	.owner   = THIS_MODULE,
	.open    = exp_cdev_open,
	.release = exp_cdev_release,
	.mmap    = exp_cdev_mmap,
	.read    = exp_cdev_read,
};

void unregister_ch_dev(void)
{
	cdev_del(exp_cdev);
	unregister_chrdev_region(exp_dt, 1);
}

void exp_char_dev(void)
{
	int ret = 0;

	alloc_chrdev_region(&exp_dt, 0, 1, "exp_cdev");
	prk("test char dev major:%d\n", MAJOR(exp_dt));

	exp_cdev = cdev_alloc();
	prfl("test_cdev=%p\n", exp_cdev);

	cdev_init(exp_cdev, &exp_chr_fops);
	ret = cdev_add(exp_cdev, exp_dt, 1);
	prfl("cdev_init return %d\n", ret);

	register_early_exit(unregister_ch_dev);
}

#endif
