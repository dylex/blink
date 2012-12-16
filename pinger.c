#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <arpa/inet.h>
#include "../pinger/pinger.h"
#include "activity.h"
#include "watch.h"
#include "pinger.h"

#define PINGER_HOST	"66.114.66.1"
#define PINGER_WAIT	(60*INTERVAL_SECOND)
#define PINGER_COLOR	RED

static int Pinger_sock = -1;
static struct ping Ping = { .time = 5000000 };

static void pinger_stop()
{
	if (Pinger_sock >= 0)
		close(Pinger_sock);
	Pinger_sock = -1;
}

static int pinger_start()
{
	struct sockaddr_un la = { AF_UNIX }, sa = { AF_UNIX, PINGER_SOCKET };

	if ((Pinger_sock = socket(PF_UNIX, SOCK_DGRAM, 0)) < 0)
		return -1;
	if (bind(Pinger_sock, &la, SUN_LEN(&la)) < 0 ||
			connect(Pinger_sock, &sa, SUN_LEN(&sa)) < 0)
		return -1;
	return 0;
}

static int pinger_send()
{
	if (send(Pinger_sock, &Ping, sizeof(Ping), 0) < 0)
	{
		fprintf(stderr, "pinger send: %m\n");
		return -1;
	}
	return 0;
}

static uint8_t pinger_color(int32_t time)
{
	if (time < 0)
		return 255;
	else if (time > 2100000)
		return 128;
	else if (time >  100000)
		return 128*(time - 100000)/2000000;
	else
		return 0;
}

static struct activity Pinger_act;

static void pinger_res(struct watch *w, uint8_t events)
{
	struct ping p;
	ssize_t r = recv(Pinger_sock, &p, sizeof(p), 0);
	if (r < 0)
		p.time = -errno;
	else if (r != sizeof(p))
		p.time = -EBADE;
	if (p.time < 0 && p.time != -ETIMEDOUT)
	{
		fprintf(stderr, "pinger res: %s\n", strerror(-p.time));
		return;
	}

	activity_rm(&Pinger_act, Pinger_act.seg.start);
	uint8_t c = pinger_color(p.time);
	Pinger_act.seg.end[PINGER_COLOR] = c;
	Pinger_act.seg.len = PINGER_WAIT/(c/20+1);
	activity_add(&Pinger_act);
}

static struct watch Pinger_watch = { .fd = -1, .events = WATCH(IN), .fun = &pinger_res };

static void pinger_run(struct activity *a)
{
	if ((Pinger_sock < 0 && pinger_start() < 0)
			|| pinger_send() < 0)
	{
		pinger_stop();
		Pinger_act.seg.start[PINGER_COLOR] = 128;
	}

	if (Pinger_watch.fd != Pinger_sock)
	{
		if (Pinger_watch.fd >= 0)
		{
			watch_rm(&Pinger_watch);
			Pinger_watch.fd = -1;
		}
		if (Pinger_sock >= 0)
		{
			Pinger_watch.fd = Pinger_sock;
			watch_add(&Pinger_watch);
		}
	}

	color_cpy(Pinger_act.seg.start, Pinger_act.seg.end);
	Pinger_act.seg.len = PINGER_WAIT;
	activity_add(&Pinger_act);
}

void pinger_init()
{
	Ping.host = inet_addr(PINGER_HOST);
	Pinger_act.fun = &pinger_run;
	activity_add(&Pinger_act);
}
