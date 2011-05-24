#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv)
{
	int fd, res, len;
	struct sockaddr_in server, client;
	char buf[1000];

	socklen_t clilen;

	memset(&server, 0x0, sizeof(server));
	server.sin_family	= AF_INET;
	server.sin_port		= htons(40000);
	server.sin_addr.s_addr	= htonl(INADDR_ANY);

	fd = socket(AF_INET, SOCK_DGRAM, 0);
	if(fd < 0) {
		fprintf(stderr, "socket failed\n");
		return 1;
	}

	res = bind(fd, (struct sockaddr *)&server, sizeof(server));
	if(res < 0) {
		fprintf(stderr, "bind failed\n");
		close(fd);
		return 1;
	}

	while(1) {
		memset(buf, 0x0, sizeof(buf));
		len = recvfrom(fd, buf, sizeof(buf), 0,
				(struct sockaddr *)&client, &clilen);
		printf("Message from: %s\n\t%s\n", inet_ntoa(client.sin_addr),
				buf);
		sendto(fd, buf, len, 0, (struct sockaddr *)&client, clilen);
	}

	close(fd);

	return 0;
}
