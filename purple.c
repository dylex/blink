#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include "activity.h"
#include "watch.h"
#include "purple.h"

#define PURPLE_PORT	6652
#define PURPLE_LED	0
#define PURPLE_COLOR	{ COLOR_MAX/2, COLOR_MAX/2, 0 }
#define PURPLE_TIME	(INTERVAL_SECOND/2)
#define PURPLE_RETRY	(300*INTERVAL_SECOND)

static signed Purple_count = -1;

static activity_fn purple_next;
static void purple_connect(void);
static void purple_retry(void);

static struct activity Purple_act = { .fun = &purple_next };
static struct activity_then Purple_blink = { { { .start = PURPLE_COLOR, .end = PURPLE_COLOR, .len = PURPLE_TIME }, .fun = activity_then }, &Purple_act };

static void purple_next(struct activity *act, enum led led)
{
	static unsigned count = 0;

	if (Purple_count == -1)
		purple_connect();
	else if (Purple_count > 0) {
		Purple_act.seg.len = PURPLE_TIME;
		if (++count >= Purple_count) {
			count = 0;
			Purple_act.seg.len *= 3;
		}
		activity_add(&Purple_blink.act, led);
	}
	else
		count = 0;
}

static void purple_update()
{
	if (Purple_count == -1)
	{
		if (activity_active(&Purple_act))
			activity_rm(&Purple_act, PURPLE_LED, NULL);
		Purple_act.seg.len = PURPLE_RETRY;
		if (!activity_active(&Purple_blink.act))
			activity_add(&Purple_act, PURPLE_LED);
	}
	else if (Purple_count > 0 && !(activity_active(&Purple_blink.act) || activity_active(&Purple_act)))
		purple_next(NULL, PURPLE_LED);
}

static void purple_in(struct watch *w, uint8_t events)
{
	unsigned char c;
	if (recv(w->fd, &c, sizeof(c), 0) <= 0) {
		fprintf(stderr, "purple recv: %m\n");
		watch_rm(w);
		return purple_retry();
	}
	Purple_count = c;
	purple_update();
}

static struct watch Purple_watch = { .fd = -1, .events = WATCH(IN), &purple_in };

static void purple_retry()
{
	if (Purple_watch.fd >= 0) {
		close(Purple_watch.fd);
		Purple_watch.fd = -1;
	}
	Purple_count = -1;
	purple_update();
}

static void purple_connect()
{
	const struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port = htons(PURPLE_PORT),
		.sin_addr = { htonl(INADDR_LOOPBACK) }
	};

	if ((Purple_watch.fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		fprintf(stderr, "purple socket: %m\n");
		return;
	}
	if (connect(Purple_watch.fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
		fprintf(stderr, "purple connect: %m\n");
		return purple_retry();
	}
	fcntl(Purple_watch.fd, F_SETFL, O_NONBLOCK);
	watch_add(&Purple_watch);
}

void purple_init()
{
	purple_connect();
}
