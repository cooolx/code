#ifdef __EXP_INPUT_FAKE__

#include <linux/module.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/sched.h>
#include <linux/mutex.h>
#include <linux/delay.h>
#include <linux/input.h>
#include <linux/platform_device.h>
#include <linux/kobject.h>
#include <linux/fs.h>
#include <asm/uaccess.h>

static struct platform_device *fake_platform_device;
static struct input_dev *idev;
static struct timer_list timer;

static void fake_timer_callback(struct timer_list * tl)
{
	input_report_key(idev, KEY_6, 1);
	input_report_key(idev, KEY_6, 0);
	input_report_key(idev, KEY_6, 1);
	input_report_key(idev, KEY_6, 0);
	input_sync(idev);
	mod_timer( &timer, jiffies+HZ );
}

static int fake_probe(struct platform_device *dev)
{
	int err = 0, i;

	prk("\n");
	idev = input_allocate_device();
	idev->name = "fake";
	idev->evbit[0] = BIT(EV_KEY);
	for (i = KEY_1; i < KEY_0; i++) {
		__set_bit(i, idev->keybit);
	}

	err = input_register_device(idev);
	if (err) {
		prk("error registering input device\n");
		goto fail;
	}

	timer_setup( &timer, fake_timer_callback, 0 );
	mod_timer( &timer, jiffies+HZ );
	return 0;

fail:
	input_free_device(idev);
	return 0;
}

static struct platform_driver fake_platform_driver = {
	.driver		= {
		.name	= "fake_input",
		.owner	= THIS_MODULE,
	},
	.probe		= fake_probe,
	.remove		= NULL,
	.shutdown	= NULL,
};

static void  fake_exit(void);
static int  fake_init(void)
{
	int err;

	err = platform_driver_register(&fake_platform_driver);
	if (err)
		return err;
	prk("\n");
	fake_platform_device = platform_device_alloc("fake_input", -1);
	if (!fake_platform_device) {
		err = -ENOMEM;
	}

	err = platform_device_add(fake_platform_device);

	register_early_exit(fake_exit);

	return 0;
}

static void fake_exit(void)
{
	prk("\n");
	del_timer(&timer);
	prk("idev=%p\n", idev);
	input_unregister_device(idev);
	platform_device_unregister(fake_platform_device);
	platform_driver_unregister(&fake_platform_driver);
}

#endif
