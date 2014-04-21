#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#if defined(__linux)
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

#define TAP_IMPL_DEFINED
#endif

#if defined(__APPLE__)
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <errno.h>
#include <string.h>

int init_tap_device(char *name)
{
  char *pathname = alloca(32);
  struct ifreq ifr;
  int fd, sock, flags;

  snprintf(pathname, 32, "/dev/%s", name);
  fd = open(pathname, O_RDWR);
  if(fd < 0) {
    printf("Open failed (%s)\n", pathname);
    return -2;
  }
  printf("fd = %d\n", fd);

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if(sock < 0) {
    printf("Socket open failed\n");
  }

  memset(&ifr, 0, sizeof(ifr));
  strncpy(ifr.ifr_name, name, sizeof(ifr.ifr_name));
  if( ioctl(sock, SIOCGIFFLAGS, &ifr) < 0 ) {
    printf("Get failed: %d\n", errno);
    return -errno;
  }
  printf("Flags: %x\n", ifr.ifr_flags);

  ifr.ifr_flags |= IFF_UP | IFF_RUNNING;
  if( ioctl(sock, SIOCSIFFLAGS, &ifr) < 0 ) {
    printf("Set failed: %d (%s)\n", errno, strerror(errno));
    return -errno;
  }

  flags = fcntl(fd, F_GETFL, 0);
  if(flags == -1) {
    printf("fcntl get failed\n");
    return -errno;
  }

  if( fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) == -1 ) {
    printf("fcntl set failed\n");
    return -errno;
  }

  printf("Everything's good! (fd = %d)\n", fd);
  return fd;
}
#define TAP_IMPL_DEFINED
#endif

#ifndef TAP_IMPL_DEFINED
#error "No TAP interface for building host!"
#endif

