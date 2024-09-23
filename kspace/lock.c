#ifdef __EXP_LOCK__

static DEFINE_SPINLOCK(exp_lock_spinlock);
static struct mutex exp_lock_mutex;

static ssize_t exp_lock_show(struct device *dev, struct device_attribute *attr, char *buf)
{
	if (in_interrupt()) {
		return sprintf(buf, "%s\n", "in interrupu");
	} else {
		return sprintf(buf, "%s\n", "not in interrupu");
	}
}

static ssize_t exp_lock_store(struct device *dev, struct device_attribute *attr, const char *buf, size_t count)
{
	int cmd;
	
	cmd = simple_strtoul(buf, NULL, 10);
	
	switch (cmd) {
	case 0:
		pr_err("lzd init spin_lock\n");
		spin_lock_init(&exp_lock_spinlock);
		mutex_init(&exp_lock_mutex);
		break;
	case 1:
		pr_err("lzd spin_lock\n");
		spin_lock(&exp_lock_spinlock);
		break;
	case 2:
		pr_err("lzd spin_unlock\n");
		spin_unlock(&exp_lock_spinlock);
		break;
	case 3:
		pr_err("lzd spin_lock_irq\n");
		spin_lock_irq(&exp_lock_spinlock);
		break;
	case 4:
		pr_err("lzd spin_unlock_irq\n");
		spin_unlock_irq(&exp_lock_spinlock);
		break;
	case 5:
		pr_err("lzd mutex_lock\n");
		mutex_lock(&exp_lock_mutex);
		break;
	case 6:
		pr_err("lzd mutex_unlock\n");
		mutex_unlock(&exp_lock_mutex);
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

static DEVICE_ATTR_RW(exp_lock);

static struct attribute *exp_lock_atts[] = {
	&dev_attr_exp_lock.attr,
	NULL
};

static const struct attribute_group exp_lock_atts_group = {
	.attrs = exp_lock_atts,
};

static const struct attribute_group *exp_lock_dev_attr_groups[] = {
	&exp_lock_atts_group,
	NULL
};

static int exp_lock_init(struct platform_device *exp_device)
{
	int ret = 0;
	int i;
	for (i = 0; exp_lock_dev_attr_groups[i]; i++) {
		ret = sysfs_create_group(&exp_device->dev.kobj, exp_lock_dev_attr_groups[i]);
		if (ret) {
			while (--i >= 0) {
				sysfs_remove_group(&exp_device->dev.kobj, exp_lock_dev_attr_groups[i]);
			}
			break;
		}
	}
	return ret;
}

#endif