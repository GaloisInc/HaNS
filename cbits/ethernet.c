#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <netinet/in.h>
#include <netinet/ether.h>
#include <linux/if_packet.h>
#include <net/ethernet.h>

int init_ethernet_device(char *name, unsigned char *mac, int *idx) {
  int fd, ret, sockopt;
  struct ifreq if_idx;

  if (name == NULL) {
    return -1;
  }

  if ((fd = socket(PF_PACKET, SOCK_RAW, IPPROTO_RAW) < 0)) {
    perror("Couldn't open socket");
    return -1;
  }

  /* clear struct */
  memset(&if_idx, 0, sizeof(struct ifreq));

  /* copy name of interface to name */
  strncpy(if_idx.ifr_name, name, IFNAMSIZ-1);

  /* set promiscuous */
  if_idx.ifr_flags |= IFF_PROMISC;

  if (ioctl(fd, SIOCGIFINDEX, &if_idx) < 0)
    perror("SIOCGIFINDEX");

  /* Get the index of the interface */
  *idx = if_idx.ifr_ifindex;

  /* Allow the socket to be reused - incase connection is closed prematurely */
  if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &sockopt, sizeof sockopt) == -1) {
	perror("setsockopt");
	close(fd);
	return -1;
  }

  /* Bind to device */
  if (setsockopt(fd, SOL_SOCKET, SO_BINDTODEVICE, name, IFNAMSIZ-1) == -1) {
	perror("SO_BINDTODEVICE");
	close(fd);
	return -1;
  }

  return fd;
}

int send_to_socket (int sockfd, const void * buf, size_t len, int * if_idx, unsigned char *mac) {

  /* Socket address */
  struct sockaddr_ll socket_address;

  /* Index of the network device */
  socket_address.sll_ifindex = *if_idx;

  /* Address length*/
  socket_address.sll_halen = ETH_ALEN;

  /* Destination MAC */
  *socket_address.sll_addr = *mac;

  /* Send packet */
  if (sendto(sockfd, buf, len, 0, (struct sockaddr*)&socket_address, sizeof(struct sockaddr_ll)) < 0)
    printf("Send failed\n");

}
