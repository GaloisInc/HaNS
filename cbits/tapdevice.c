#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#if defined(__linux)
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

int init_tap_device(char *name, unsigned char *mac) {
  int fd, ret, sock;
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

  // generate a MAC address based on the one in the linux side.
  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if(sock == -1) {
    close(fd);
    return -4;
  }

  if(ioctl(sock,SIOCGIFHWADDR,&ifr) == -1) {
    close(fd);
    close(sock);
    return -5;
  }

  // flip the last bit of the mac to generate a new one on the same link
  memcpy(mac, ifr.ifr_hwaddr.sa_data, 6);
  mac[5] ^= 1;

  close(sock);

  return fd;
}

#define TAP_IMPL_DEFINED
#endif

#if defined(__APPLE__)
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <ifaddrs.h>
#include <errno.h>
#include <string.h>

int init_tap_device(char *name, char *mac)
{
  char *pathname = alloca(32);
  struct ifreq ifr;
  int fd, sock, flags, found, i;
  struct ifaddrs *iflist;
  struct ifaddrs *cur;
  struct sockaddr_dl *sdl;

  snprintf(pathname, 32, "/dev/%s", name);
  fd = open(pathname, O_RDWR);
  if(fd < 0) {
    printf("Open failed (%s) (%d)\n", pathname, fd);
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

  found = 0;
  if(getifaddrs(&iflist) == 0) {
    for(cur = iflist; cur; cur = cur->ifa_next) {
      if((cur->ifa_addr->sa_family == AF_LINK) &&
              cur->ifa_addr &&
              strcmp(cur->ifa_name, name) == 0) {
        sdl = (struct sockaddr_dl*)cur->ifa_addr;
        memcpy(mac, LLADDR(sdl), sdl->sdl_alen);

        // flip the last bit of the mac address to generate a new address.
        mac[5] ^= 1;
        found = 1;
        break;
      }
    }
  }

  if(found == 0) {
    return -2;
  }



  close(sock);

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

