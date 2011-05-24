
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

int init_tap_device(char *name) {
	int fd, ret;
	struct ifreq ifr;

	if(name == NULL) {
		return -1;
	}

	fd = open("/dev/net/tun", O_RDWR);
	if(fd < 0) {
		return -2;
	}

	memset(&ifr, 0x0, sizeof(struct ifreq));
	ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
	strncpy(ifr.ifr_name, name, IFNAMSIZ);

	ret = ioctl(fd, TUNSETIFF, (void*) &ifr);
	if(ret != 0) {
		close(fd);
		return -3;
	}

	return fd;
}
