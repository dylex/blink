#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include "watch.h"
#include "activity.h"
#include "mail.h"
#include "purple.h"
#include "remote.h"

#define REMOTE_MASK_MAIL	0x80
#define REMOTE_MASK_PURPLE	0x7F
#define CLIENT_RETRY	(300*INTERVAL_SECOND)

static struct sockaddr_in Connect_addr = {
	.sin_family = AF_INET,
	.sin_addr = { 
#if __BYTE_ORDER == __LITTLE_ENDIAN
		__bswap_constant_32(INADDR_LOOPBACK) 
#else // this won't compile, sorry:
		htonl(INADDR_LOOPBACK) 
#endif
	}
};

static inline void client_ready(struct watch *w)
{
	w->events = WATCH(IN);
	watch_add(w);
}

static void client_retry(void);

static inline void connect_error()
{
	if (errno != ECONNREFUSED)
		fprintf(stderr, "remote connect: %m\n");
	return client_retry();
}

static void client_in(struct watch *w, uint8_t events)
{
	if (w->events == WATCH(OUT))
	{
		watch_rm(w);

		socklen_t len = sizeof(errno);
		if (getsockopt(w->fd, SOL_SOCKET, SO_ERROR, &errno, &len) < 0 || errno)
			return connect_error();

		return client_ready(w);
	}

	unsigned char c;
	if (recv(w->fd, &c, sizeof(c), 0) <= 0) {
		fprintf(stderr, "remote recv: %m\n");
		watch_rm(w);
		return client_retry();
	}

	purple_update(c & REMOTE_MASK_PURPLE);

	static bool last_mail = 0;
	bool mail = !!(c & REMOTE_MASK_MAIL);
	if (mail != last_mail) {
		Mail_count += mail - last_mail;
		last_mail = mail;
		mail_update();
	}
}

static struct watch Client = { .fd = -1, .fun = &client_in };

static void client_connect(struct activity *act, enum led led) 
{
	if ((Client.fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		fprintf(stderr, "remote socket: %m\n");
		return;
	}
	fcntl(Client.fd, F_SETFL, O_NONBLOCK);
	if (connect(Client.fd, (struct sockaddr *)&Connect_addr, sizeof(Connect_addr)) < 0) {
		if (errno == EINPROGRESS)
		{
			Client.events = WATCH(OUT);
			watch_add(&Client);
			return;
		}
		return connect_error();
	}

	return client_ready(&Client);
}

static struct activity Client_retry = { { .len = CLIENT_RETRY }, .fun = &client_connect };

static void client_retry()
{
	if (Client.fd >= 0) {
		close(Client.fd);
		Client.fd = -1;
	}

	activity_add(&Client_retry, 0);
}


static void server_out(struct watch *w, uint8_t events)
{
	watch_rm(w);

	unsigned char c = 0;
	if (Purple_count >= 0)
		c |= Purple_count > REMOTE_MASK_PURPLE ? REMOTE_MASK_PURPLE : Purple_count;
	if (Mail_count > 0)
		c |= REMOTE_MASK_MAIL;
	if (send(w->fd, &c, sizeof(c), 0) <= 0) {
		// fprintf(stderr, "remote send: %m\n");
		close(w->fd);
		w->fd = -1;
	}
}

static struct watch Server = { .fd = -1, .events = WATCH(OUT), &server_out };

void remote_update()
{
	if (Server.fd < 0)
		return;
	if (watch_active(&Server))
		return;
	watch_add(&Server);
}

static void listen_in(struct watch *w, uint8_t events)
{
	struct sockaddr_in addr;
	socklen_t addrlen = sizeof(addr);

	if (Server.fd >= 0) {
		if (watch_active(&Server))
			watch_rm(&Server);
		close(Server.fd);
		Server.fd = -1;
	}
	if ((Server.fd = accept(w->fd, (struct sockaddr *)&addr, &addrlen)) < 0) {
		fprintf(stderr, "remote accept: %m\n");
		return;
	}

	shutdown(Server.fd, SHUT_RD);
	fcntl(Server.fd, F_SETFL, O_NONBLOCK);

	watch_add(&Server);
}

static struct watch Listener = { .fd = -1, .events = WATCH(IN), &listen_in };

static int remote_listen(uint16_t port)
{
	const int on = 1;
	const struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port = htons(port),
		.sin_addr = { htonl(INADDR_LOOPBACK) }
	};

	if ((Listener.fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		return -1;
	setsockopt(Listener.fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
	if (bind(Listener.fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
			listen(Listener.fd, 1) < 0)
	{
		close(Listener.fd);
		Listener.fd = -1;
		return -1;
	}

	fcntl(Listener.fd, F_SETFL, O_NONBLOCK);

	watch_add(&Listener);
	return 0;
}

int remote_init(uint16_t listen_port, uint16_t connect_port)
{
	if (connect_port)
	{
		assert(connect_port != listen_port);
		Connect_addr.sin_port = htons(connect_port);
		client_connect(NULL, 0);
	}

	if (listen_port)
		return remote_listen(listen_port);
	return 0;
}
