#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <math.h>
#include "../pinger/pingdev.h"
#include "activity.h"
#include "watch.h"
#include "pinger.h"

#define PINGDEV	"/dev/ping"
#define PINGER_COLOR	RED

static uint8_t pinger_color(float time)
{
	if (isinf(time))
		return 255;
	else if (time > 2.1)
		return 128;
	else if (time > 0.1)
		return 64*(time - 0.1);
	else
		return 0;
}

static struct activity Pinger_inc = { { .end = { [PINGER_COLOR] = 255 } }, .led = LED_1 };
static struct activity_then Pinger_act = { { .fun = activity_then }, &Pinger_inc };

static void pinger_res(struct watch *w, uint8_t events);
static struct watch Pinger_watch = { .events = WATCH(IN), .fun = &pinger_res };

static void pinger_res(struct watch *w, uint8_t events)
{
	float p = 0;
	int r = ioctl(w->fd, PINGDEV_GET_PING, &p);
	if (r == -1)
	{
		fprintf(stderr, "pinger: %m\n");
		watch_rm(&Pinger_watch);
		r = pinger_init();
		if (r < 0)
			fprintf(stderr, "pinger_init: %m\n");
		return;
	}

	if (activity_active(&Pinger_act.act))
		activity_rm(&Pinger_act.act, Pinger_act.act.seg.start);
	else if (activity_active(&Pinger_inc))
		activity_rm(&Pinger_inc, Pinger_act.act.seg.start);
	else
		color_cpy(Pinger_act.act.seg.start, color_zero);
	Pinger_inc.seg.start[PINGER_COLOR] = Pinger_act.act.seg.end[PINGER_COLOR] = pinger_color(p);
	activity_add(&Pinger_act.act);
}

int pinger_init()
{
	int pd = open(PINGDEV, O_RDONLY);
	if (pd < 0)
		return pd;
	int i = ioctl(pd, PINGDEV_GET_INTERVAL);
	if (i < 0)
	{
		close(pd);
		return i;
	}
	if (i == 0)
	{
		close(pd);
		errno = ENOSYS;
		return -1;
	}
	Pinger_watch.fd = pd;
	Pinger_act.act.seg.len = Pinger_inc.seg.len = i*INTERVAL_SECOND;
	watch_add(&Pinger_watch);
	return 0;
}
