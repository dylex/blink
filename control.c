#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <fcntl.h>
#include <stdio.h>

#include "activity.h"
#include "watch.h"
#include "control.h"

static int Control_sock = -1;
static color_t Control_color;

static void control_color_set(color_t s)
{
	color_t p = {}, m = {};
	unsigned c;
	for_color (c)
		if (s[c] > Control_color[c])
			p[c] = s[c] - Control_color[c];
		else
			m[c] = Control_color[c] - s[c];
	base_rm(m);
	base_add(p);
	color_cpy(Control_color, s);
}

static void control_run(struct watch *w, uint8_t events)
{
	union {
		char buf[256];
		enum cmd cmd;
		struct cmd_color cmd_color;
	} cmdbuf;

	ssize_t z = recv(Control_sock, cmdbuf.buf, sizeof(cmdbuf), 0);
	if (z < 0)
	{
		fprintf(stderr, "control recv: %m\n");
		return;
	}
#define CHECK_LEN(T) \
	if (z < sizeof(cmdbuf.T)) \
	{ \
		fprintf(stderr, "control len: %zd for %s\n", z, #T); \
		return; \
	}
	CHECK_LEN(cmd);
	unsigned c;
	color_t ct = {};
	if (z >= sizeof(cmdbuf.cmd_color))
		color_cpy(ct, cmdbuf.cmd_color.color);
	switch (cmdbuf.cmd)
	{
		case CMD_NULL:
			break;

		case CMD_SET:
			control_color_set(ct);
			break;

		case CMD_ADD:
			CHECK_LEN(cmd_color);
			for_color (c)
				ct[c] = Control_color[c] + ct[c];
			control_color_set(ct);
			break;

		case CMD_SUB:
			CHECK_LEN(cmd_color);
			for_color (c)
				ct[c] = Control_color[c] - ct[c];
			control_color_set(ct);
			break;
	}
#undef CHECK_LEN
}

static struct watch Control_watch = { .events = WATCH(IN), .fun = &control_run };

int control_init()
{
	struct sockaddr_un sa = { AF_UNIX, CONTROL_SOCKET };

	if ((Control_sock = socket(PF_UNIX, SOCK_DGRAM, 0)) < 0)
		return Control_sock;
	if (fcntl(Control_sock, F_SETFL, O_NONBLOCK) < 0 ||
			fcntl(Control_sock, F_SETFD, FD_CLOEXEC) < 0)
		return -1;
	unlink(sa.sun_path);
	mode_t omask = umask(0177);
	int r = bind(Control_sock, (struct sockaddr *)&sa, SUN_LEN(&sa));
	umask(omask);
	if (r < 0)
		return r;
	Control_watch.fd = Control_sock;
	watch_add(&Control_watch);
	return 0;
}
