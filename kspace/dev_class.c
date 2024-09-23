#ifdef __EXP_DEV_CLASS__

#include <linux/device.h>

static struct class* class_test;
static struct device* test_dev;

static ssize_t exp_dev_class_show(struct device *dev, 
		struct device_attribute *attr, char *buf)
{
	return sprintf(buf, "show\n");
}

static ssize_t exp_dev_class_store(struct device *dev,
		struct device_attribute *attr, const char *buf, size_t size)
{
	return size;
}

static DEVICE_ATTR_RW(exp_dev_class);

static struct attribute *dev_class_attrs[] = {
	&dev_attr_exp_dev_class.attr,
	NULL,
};
ATTRIBUTE_GROUPS(dev_class);

static void cleanup()
{
	device_destroy(class_test, 0);
	class_destroy(class_test);
}

void exp_dev_class(void)
{
	class_test = class_create(THIS_MODULE, "class_test");
	class_test->dev_groups = dev_class_groups;

	test_dev = device_create(class_test, NULL, 0, NULL, "test_dev");
	register_early_exit(cleanup);
}
#endif
