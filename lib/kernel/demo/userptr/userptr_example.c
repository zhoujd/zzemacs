// userptr_example.c
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/miscdevice.h>
#include <linux/uaccess.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/pagemap.h>

#define DEVICE_NAME "userptr_example"
#define IOCTL_GET_BUFFER _IOWR('u', 1, struct user_buffer)

struct user_buffer {
    void __user *ptr;   // user-space pointer
    size_t len;         // length in bytes
};

static long int example_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{
    struct user_buffer ubuf;
    struct page **pages;
    int nr_pages;
    int i, ret;
    size_t offset;
    char *kernel_addr;

    if (cmd != IOCTL_GET_BUFFER)
        return -ENOTTY;

    // 1. Copy the user_buffer structure from user space
    if (copy_from_user(&ubuf, (void __user *)arg, sizeof(ubuf)))
        return -EFAULT;

    if (!ubuf.ptr || ubuf.len == 0)
        return -EINVAL;

    // 2. Calculate how many pages cover the user buffer
    nr_pages = (offset_in_page(ubuf.ptr) + ubuf.len + PAGE_SIZE - 1) >> PAGE_SHIFT;

    // 3. Allocate array for page pointers
    pages = kmalloc_array(nr_pages, sizeof(*pages), GFP_KERNEL);
    if (!pages)
        return -ENOMEM;

    // 4. Pin user pages (long-term pin, suitable for DMA)
    ret = pin_user_pages((unsigned long)ubuf.ptr, nr_pages,
                         FOLL_WRITE, pages, NULL);
    if (ret != nr_pages) {
        if (ret > 0)
            unpin_user_pages(pages, ret);
        kfree(pages);
        return -EFAULT;
    }

    // 5. Access the buffer (e.g., print first few bytes)
    pr_info("User buffer pinned. Length = %zu\n", ubuf.len);

    // Map the first page to kernel virtual address
    offset = offset_in_page(ubuf.ptr);
    kernel_addr = kmap_local_page(pages[0]) + offset;
    pr_info("First 16 bytes: ");
    for (i = 0; i < min(16UL, ubuf.len - offset); i++)
        pr_cont("%02x ", kernel_addr[i]);
    pr_cont("\n");
    kunmap_local(kernel_addr - offset);  // unmap the page (restore original pointer)

    // 6. Release pages
    unpin_user_pages(pages, nr_pages);
    kfree(pages);

    return 0;
}

static struct file_operations example_fops = {
    .owner          = THIS_MODULE,
    .unlocked_ioctl = example_ioctl,
};

static struct miscdevice example_miscdev = {
    .minor = MISC_DYNAMIC_MINOR,
    .name  = DEVICE_NAME,
    .fops  = &example_fops,
};

static int __init example_init(void)
{
    return misc_register(&example_miscdev);
}

static void __exit example_exit(void)
{
    misc_deregister(&example_miscdev);
}

module_init(example_init);
module_exit(example_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("User pointer (userptr) example");
MODULE_AUTHOR("Your Name");
