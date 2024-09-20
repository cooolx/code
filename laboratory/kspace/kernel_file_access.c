#ifdef __EXP_KERNEL_FILE_ACCESS__
void kernel_file_access(void)
{
	mm_segment_t old_fs;
	loff_t pos = 0;
	struct file* p = filp_open("/root/1.txt", O_CREAT | O_RDWR, 0);

	printk("p=%p\n", p);
	if ( !p )
		return;
	
	old_fs = get_fs();
	set_fs(KERNEL_DS);
	
	//vfs_write(p, "hello", 6, &pos);
	kernel_write(p, "hello", 6, &pos);
	set_fs(old_fs);
	filp_close(p, NULL);
}

MODULE_IMPORT_NS(VFS_internal_I_am_really_a_filesystem_and_am_NOT_a_driver);

#endif
