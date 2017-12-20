#ifndef __ETHERNET_DEVICE_H
#define __ETHERNET_DEVICE_H

int init_ethernet_device(char *name, unsigned char *mac);
int send_to_socket (int sockfd, const void *buf, size_t len, int *if_idx, unsigned char *mac);

#endif
