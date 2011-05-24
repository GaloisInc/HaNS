#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv)
{
	int fd, res, len;
	struct sockaddr_in server;
	char buf[1000] = "Hello, world.";

	if(argc < 2) {
		fprintf(stderr, "usage: %s <ip>\n", argv[0]);
		return 1;
	}

	memset(&server, 0x0, sizeof(server));
	server.sin_family	= AF_INET;
	server.sin_port		= htons(40000);

	res = inet_pton(AF_INET, argv[1], &server.sin_addr);
	if(res < 0) {
		fprintf(stderr, "Unable to parse ip\n");
		return 1;
	}

	fd = socket(AF_INET, SOCK_DGRAM, 0);
	if(fd < 0) {
		fprintf(stderr, "socket failed\n");
		return 1;
	}

	sendto(fd, buf, sizeof(buf), 0,
			(struct sockaddr *)&server, sizeof(server));

	len = recvfrom(fd, buf, sizeof(buf), 0, NULL, NULL);

	printf("received: %s\n", buf);

	return 0;
}
