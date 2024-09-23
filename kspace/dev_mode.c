//-----------------------------------------------------------------------------------
#ifdef __EXP_DEV_MODEL__

#include <linux/device.h>

static struct kobject dev_kobj;
static struct kobject dev_kobj1;


static struct kset dev_kt;
static struct kobject dev_kobj2;
static struct kobject dev_kobj3;
static struct kobject dev_kobj4;

static struct kobject *test_kobj;

void obj_test_release(struct kobject *kobject)
{
	printk("eric_test: release .\n");
}

ssize_t eric_test_show(struct kobject *kobject, struct attribute *attr,char *buf)
{
	printk("have show.\n");
	printk("attrname:%s.\n", attr->name);
	sprintf(buf,"%s\n",attr->name);
	return strlen(attr->name)+2;
}
 
ssize_t eric_test_store(struct kobject *kobject,struct attribute *attr,const char *buf, size_t count)
{
	printk("havestore\n");
	printk("write: %s\n",buf);
	return count;
}
 
struct attribute test_attr = {
	.name = "eric_xiao",
	.mode = S_IRWXUGO,
};
 
static struct attribute *dev_attrs[] = {
	&test_attr,
	NULL,
};
 
 
struct sysfs_ops obj_test_sysops =
{
	.show = eric_test_show,
	.store = eric_test_store,
};

struct kobj_type dev_ktype =
{
	.release = obj_test_release,
	.sysfs_ops=&obj_test_sysops,
	.default_attrs=dev_attrs,
};

static ssize_t test_show(struct device* dev, struct device_attribute* attr, char* buff)
{
	int ret = sprintf(buff, "test");
	prk("buff=%s\n", buff);
	return ret;
}

static ssize_t test_store(struct device* dev, struct device_attribute* attr, const char* buff, size_t length)
{
	prk("buff=%s\n", buff);
	return length;
}

static DEVICE_ATTR(test, S_IWUSR|S_IRUGO, test_show, test_store);

void unregister_the_dev(void);
//---------------------------------------------
void exp_dev_model(void)
{
	//kobject_init_and_add(&kobj,&ktype,NULL,"eric_test");
	kobject_init( &dev_kobj, &dev_ktype );
	kobject_add( &dev_kobj, NULL, "top" );

	kobject_init( &dev_kobj1, &dev_ktype );
	kobject_add( &dev_kobj1, &dev_kobj, "kobj1" );

	test_kobj = kobject_create_and_add("test", &dev_kobj);
	sysfs_create_file(test_kobj, &dev_attr_test.attr);

	//--------------------------------
	dev_kt.kobj.parent = &dev_kobj;
	dev_kt.kobj.ktype = &dev_ktype;
	kobject_set_name( &dev_kt.kobj, "kt" );
	kset_register( &dev_kt );

	kobject_init( &dev_kobj2, &dev_ktype );
	kobject_add( &dev_kobj2, &dev_kt.kobj, "kobj2" );

	kobject_init( &dev_kobj3, &dev_ktype );
	kobject_add( &dev_kobj3, &dev_kt.kobj, "kobj3" );

	kobject_init( &dev_kobj4, &dev_ktype );
	kobject_add( &dev_kobj4, &dev_kt.kobj, "kobj4" );

	register_early_exit(unregister_the_dev);
}

void unregister_the_dev()
{
	sysfs_remove_file( test_kobj, &dev_attr_test.attr );
	kobject_del( test_kobj );
	kobject_del( &dev_kobj1 );
	kobject_del( &dev_kobj );

	kobject_del( &dev_kobj2 );
	kobject_del( &dev_kobj3 );
	kobject_del( &dev_kobj4 );
	kset_unregister( &dev_kt );
}
#endif
