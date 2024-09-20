

static ssize_t exp_mem_show(struct device *dev, struct device_attribute *attr, char *buf)
{
	if (in_interrupt()) {
		return sprintf(buf, "%s\n", "in interrupu");
	} else {
		return sprintf(buf, "%s\n", "not in interrupu");
	}
}

static ssize_t exp_mem_store(struct device *dev, struct device_attribute *attr, const char *buf, size_t count)
{
	int i;
	int cmd = 0;
	int param = 100;
	char *str;
	char *pos;
	void *p;
	struct page *pg;

	pr_err("lzd %s:%d\n", __FILE__, __LINE__);
	str = (char*)buf;
	pos = strchr(str, ' ');
	if (pos) {
		*pos = 0;
		kstrtoint(str, 10, &cmd);
		str = ++pos;
		kstrtoint(str, 10, &param);
	}
	pr_info("lzd cmd=%d param=%d\n", cmd, param);
	switch (cmd) {
	case 0:
		for (i=0; i<param; i++) {
			p = kmalloc(32, GFP_KERNEL);
		}
		break;
	case 1:
		for (i=0; i<param; i++) {
			p = vmalloc(32);
		}
		break;
	case 2:
		for (i=0; i<param; i++) {
			pg = alloc_page(GFP_KERNEL);
		}
		break;
	case 3:
		break;
	case 4:
		break;
	case 5:
		break;
	case 6:
		break;
	case 7:
		break;
	case 8:
		break;
	default:
		break;
	}
	return count;
}

static DEVICE_ATTR_RW(exp_mem);

static struct attribute *exp_mem_atts[] = {
	&dev_attr_exp_mem.attr,
	NULL
};

static const struct attribute_group exp_mem_atts_group = {
	.attrs = exp_mem_atts,
};

static const struct attribute_group *exp_mem_dev_attr_groups[] = {
	&exp_mem_atts_group,
	NULL
};

static int exp_mem_init(struct platform_device *exp_device)
{
	int ret = 0;
	int i;
	for (i = 0; exp_mem_dev_attr_groups[i]; i++) {
		ret = sysfs_create_group(&exp_device->dev.kobj, exp_mem_dev_attr_groups[i]);
		if (ret) {
			while (--i >= 0) {
				sysfs_remove_group(&exp_device->dev.kobj, exp_mem_dev_attr_groups[i]);
			}
			break;
		}
	}
	return ret;
}
