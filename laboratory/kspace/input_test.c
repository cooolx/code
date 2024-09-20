#ifdef __EXP_INPUT__

struct kobject input_obj;

void input_test_release(struct kobject *kobject)
{
	printk("release\n");
}

ssize_t input_test_show(struct kobject *kobject, struct attribute *attr,char *buf)
{
	printk("write: %s\n",buf);
	return 0;
}

ssize_t input_test_store(struct kobject *kobject,struct attribute *attr,const char *buf, size_t count)
{
        printk("write: %s\n",buf);
        return count;
}


struct sysfs_ops input_test_sysops =
{
	.show  = input_test_show,
	.store = input_test_store,
};

struct attribute input_attr = {
	.name = "input",
	.mode = S_IRWXUGO,
};

static struct attribute *input_attrs[] = {
	&input_attr,
	NULL,
};

struct kobj_type input_ktype =
{
	.release = input_test_release,
	.sysfs_ops=&input_test_sysops,
	.default_attrs=input_attrs,
};

void input_test_exit(void)
{
	kobject_del(&input_obj);
}
void input_test(void)
{
	kobject_init( &input_obj, &input_ktype );
	kobject_add( &input_obj, NULL, "input_test" );
	register_early_exit(input_test_exit);
}

#endif

