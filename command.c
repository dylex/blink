#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <fcntl.h>
#include <stdio.h>

#include "activity.h"
#include "watch.h"
#include "command.h"

static int Command_sock = -1;
static color_t Command_color[LED_COUNT];

static void command_color_set(color_t s, enum led led)
{
	base_rm(Command_color[led], led);
	color_cpy(Command_color[led], s);
	base_add(Command_color[led], led);
}

static void command_sequence(struct segment *seq, unsigned n, enum led led)
{
	unsigned i;
	struct activity_then *a;
	a = calloc(n, sizeof(*a));
	if (!a)
	{
		fprintf(stderr, "command_sequence(%u): %m\n", n);
		return;
	}
	a[0].act.seg = seq[n-1];
	a[0].act.fun = activity_free;
	a[0].act.led = led;
	for (i = 1; i < n; i ++)
	{
		a[i].act.seg = seq[n-i-1];
		a[i].act.fun = &activity_then;
		a[i].then = &a[i-1].act;
		a[i].act.led = led;
	}
	activity_add(&a[n-1].act);
}

static void command_run(struct watch *w, uint8_t events)
{
	union {
		char buf[256];
		enum command cmd;
		struct command_color cmd_color;
		struct command_color_mask cmd_color_mask;
		struct command_sequence cmd_seq;
	} cmdbuf;

	ssize_t z = recv(Command_sock, cmdbuf.buf, sizeof(cmdbuf), 0);
	if (z < 0)
	{
		fprintf(stderr, "command recv: %m\n");
		return;
	}
#define CHECK_LEN(T) \
	if (z < sizeof(cmdbuf.T)) \
	{ \
		fprintf(stderr, "command len: %zd for %s\n", z, #T); \
		return; \
	}
	CHECK_LEN(cmd);
	enum color c;
	color_t ct = {};
	enum led led = 0;
	if (z >= sizeof(cmdbuf.cmd_color))
	{
		color_cpy(ct, cmdbuf.cmd_color.color);
		if ((led = cmdbuf.cmd_color.led) >= LED_COUNT)
			return;
	}
	switch (cmdbuf.cmd)
	{
		case COMMAND_NULL:
			break;

		case COMMAND_COLOR_SET:
			if (z >= sizeof(cmdbuf.cmd_color_mask))
				for_color (c)
					ct[c] = (ct[c] & cmdbuf.cmd_color_mask.mask[c]) | (Command_color[led][c] & ~cmdbuf.cmd_color_mask.mask[c]);
			command_color_set(ct, led);
			break;

		case COMMAND_COLOR_ADD:
			CHECK_LEN(cmd_color);
			for_color (c)
				ct[c] = Command_color[led][c] + ct[c];
			command_color_set(ct, led);
			break;

		case COMMAND_COLOR_SUB:
			CHECK_LEN(cmd_color);
			for_color (c)
				ct[c] = Command_color[led][c] - ct[c];
			command_color_set(ct, led);
			break;

		case COMMAND_SEQUENCE:
			command_sequence(cmdbuf.cmd_seq.seq, (z - offsetof(struct command_sequence, seq))/sizeof(*cmdbuf.cmd_seq.seq), led);
			break;
	}
#undef CHECK_LEN
}

static struct watch Command_watch = { .events = WATCH(IN), .fun = &command_run };

int command_init()
{
	struct sockaddr_un sa = { AF_UNIX, COMMAND_SOCKET };

	if ((Command_sock = socket(PF_UNIX, SOCK_DGRAM, 0)) < 0)
		return Command_sock;
	if (fcntl(Command_sock, F_SETFL, O_NONBLOCK) < 0 ||
			fcntl(Command_sock, F_SETFD, FD_CLOEXEC) < 0)
		return -1;
	unlink(sa.sun_path);
	mode_t omask = umask(0177);
	int r = bind(Command_sock, (struct sockaddr *)&sa, SUN_LEN(&sa));
	umask(omask);
	if (r < 0)
		return r;
	Command_watch.fd = Command_sock;
	watch_add(&Command_watch);
	return 0;
}
