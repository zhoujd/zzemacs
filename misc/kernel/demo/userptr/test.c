// test.c
#include <stdio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <fcntl.h> // for open
#include <unistd.h> // for close

struct user_buffer {
    void *ptr;
    size_t len;
};

#define IOCTL_GET_BUFFER _IOWR('u', 1, struct user_buffer)

int main()
{
    int fd = open("/dev/userptr_example", O_RDWR);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    char data[] = "Hello from user space!";
    struct user_buffer ubuf = { .ptr = data, .len = sizeof(data) };

    if (ioctl(fd, IOCTL_GET_BUFFER, &ubuf) < 0) {
        perror("ioctl");
        close(fd);
        return 1;
    }

    close(fd);
    return 0;
}
