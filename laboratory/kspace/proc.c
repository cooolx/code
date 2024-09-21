#ifdef __EXP_PROC__

#include <linux/fs.h>
#include <linux/proc_fs.h>




#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/proc_fs.h>
#include <linux/string.h>
#include <linux/vmalloc.h>
#include <asm/uaccess.h>

static struct proc_dir_entry * exp_proc_file;
static mode_t exp_proc_mode = 0644;

ssize_t exp_proc_read( struct file *file, char __user *data, size_t len, loff_t *ppos )
{
	prfl("\n");
	return 0;
}

ssize_t exp_proc_write(  struct file *file, const char __user *data, size_t len, loff_t *ppos  )
{
	prfl("\n");
	return len;
}

static const struct proc_ops exp_proc_fops = {
	.proc_read = exp_proc_read,
	.proc_write = exp_proc_write,
};

void unregister_proc(void)
{
	prfl("\n");
	remove_proc_entry("exp_proc", NULL);
}

void exp_create_proc(void)
{
	exp_proc_file = proc_create("exp_proc", exp_proc_mode, NULL, &exp_proc_fops);
	register_early_exit(unregister_proc);
}

#endif
