#include <sys/io.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <utmp.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include "ppldd.h"

#define USERNAME	"dylan"
#define MAILFILE	"/home/" USERNAME "/mail/spool"
#define LOADAVGFILE	"/proc/loadavg"
#define TEMPFILE	"/sys/bus/i2c/devices/0-0290/temp2_input"
#define TEMPWARN	55000
#define PINGPROG	"/usr/sbin/fping -eA -r 1 64.81.235.104 128.242.125.65"

#define LOADLEDNUM	1
#define USERSLEDNUM	0//3
#define TEMPLEDNUM	4
#define MAILLEDNUM	5
#define PINGLEDNUM	8

void die(const char *, ...) __attribute__((noreturn, format(printf, 1, 2)));

struct event;
typedef struct timeval event_when;
typedef void event_fn(struct event *);
typedef unsigned long event_data;
struct event {
	event_when when;
	event_fn *fn;
	/* event_data data; */
	struct event *next;
};

void schedule(void) __attribute__((noreturn));

/********************************************************** infrastructure */

static int Dev;

void init()
{
	if ((Dev = open("/dev/" PPLDD_DEVICE, O_RDONLY)) == -1)
		die("open /dev/%s: %m", PPLDD_DEVICE);
}

void fini()
{
	close(Dev);
}

void die(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "pplddd: ");
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);
	fini();
	exit(1);
}

#define MAX(X, Y) ({ typeof(X) _x = X; typeof(Y) _y = Y; _x >= _y ? _x : _y; })
#define MIN(X, Y) ({ typeof(X) _x = X; typeof(Y) _y = Y; _x <= _y ? _x : _y; })

static inline void led_ctl(int led, int req)
{
	if (ioctl(Dev, req, led) == -1)
		die("ppldd ioctl %d: %m", req);
}
#define led_set(LED, VAL) 	led_ctl(LED, (VAL) ? PPLDD_IOC_LED_ON : PPLDD_IOC_LED_OFF)
#define led_tog(LED) 		led_ctl(LED, PPLDD_IOC_LED_TOG)
#define led_on(LED) 		led_set(LED, 1)
#define led_off(LED) 		led_set(LED, 0)

static struct event *Events = NULL;
struct timeval Now;

void schedule()
{
	while (Events)
	{
		gettimeofday(&Now, NULL);
		if (timercmp(&Events->when, &Now, >))
		{
			struct timeval delta;
			timersub(&Events->when, &Now, &delta);
			struct timespec sleep = { delta.tv_sec, 1000*delta.tv_usec };
			if (nanosleep(&sleep, NULL) != 0)
				die("nanosleep: %m");
			gettimeofday(&Now, NULL);
		}
		struct event *e = Events;
		Events = e->next;
		e->next = NULL;
		e->fn(e);
	}
	die("schedule: no scheduled events");
}

void add_event(struct event *ev)
{
	if (ev->next)
		die("trying to re-add event");

	struct event **e = &Events;
	while (*e && timercmp(&(*e)->when, &ev->when, <))
		e = &(*e)->next;
	ev->next = *e;
	*e = ev;
}

void add_event_in(struct event *ev, unsigned long usec)
{
	struct timeval delta = { usec / 1000000, usec % 1000000 };
	timeradd(&Now, &delta, &ev->when);
	return add_event(ev);
}

#define INIT_LED_DELAY 125000L
void init_leds()
{
	int led = 1;
	led_tog(led);
	usleep(INIT_LED_DELAY);
	for (led = 2; led <= PPLDD_LED_COUNT; led++)
	{
		led_tog(led);
		usleep(INIT_LED_DELAY);
		led_tog(led-1);
	}
	usleep(INIT_LED_DELAY);
	led_tog(led-1);
}

/********************************************************** data gathering */

int checkfile(char *filename)
{
	struct stat s;

	if (stat(filename, &s))
	{
		perror(filename);
		return 0;
	}
	return (s.st_mtime >= s.st_atime);
}

int loadavg()
{
	FILE *f;
	float la;

	if (!(f = fopen(LOADAVGFILE, "r")))
		die("open %s: %m", LOADAVGFILE);
	if (fscanf(f, "%*f %f %*f ", &la) < 1)
		die("read %s: parse error", LOADAVGFILE);
	fclose(f);
	return 100*la;
}

int temp()
{
	FILE *f;
	int t;
	static bool temp_failed = false;

	if (!(f = fopen(TEMPFILE, "r")))
	{
		if (!temp_failed)
		{
			fprintf(stderr, "pplddd: open %s: %m\n", TEMPFILE);
			temp_failed = true;
		}
		return -1;
	}
	temp_failed = false;
	if (fscanf(f, "%d", &t) < 1)
		die("read %s: parse error", TEMPFILE);
	fclose(f);
	return t;
}

int user_idle_time(struct utmp *u)
{
	struct stat ttystat;
	char tty[strlen("/dev/") + strlen(u->ut_line) + 1];
	sprintf(tty, "/dev/%s", u->ut_line);
	if (stat(tty, &ttystat) == -1)
		return -1;
	return Now.tv_sec - ttystat.st_atime;
}

int users()
{
	struct utmp *u;
	int me = 0;
	int count = 0;

	setutent();
	while ((u = getutent()))
		if ((u->ut_type == USER_PROCESS) && (u->ut_user[0]))
		{
			if (!strcmp(u->ut_user, USERNAME))
				me++;
			count++;
		}
	endutent();
	return count - MIN(me, 1);
}

int pingstat()
{
	FILE *f;
	float t, tt = 0;
	char s[256];

	if (!(f = popen(PINGPROG, "r")))
		die("ping '%s': %m", PINGPROG);
	while (fgets(s, 255, f))
		if (sscanf(s, "%*[0-9.] is alive (%f ms)", &t) == 1)
			tt += t;
	if (pclose(f))
		return -1;
	return 10 * tt;
}

/********************************************************** running */

void pingdone(struct event *event)
{
	led_off(PINGLEDNUM);
}

void minutely(struct event *event)
{
	static struct event minutely_event = { .fn = minutely };
	static struct event pingdone_event = { .fn = pingdone };

#if USERSLEDNUM
	led_set(USERSLEDNUM, users());
#endif

#if PINGLEDNUM
	if (!pingdone_event.next)
	{
		led_on(PINGLEDNUM);
		int ping = pingstat();
		if (ping >= 0)
			add_event_in(&pingdone_event, 1000 * MIN(ping, 5000));
	}
#endif

#if TEMPLEDNUM
	led_set(TEMPLEDNUM, temp() > TEMPWARN);
#endif
#if MAILLEDNUM
	led_set(MAILLEDNUM, checkfile(MAILFILE));
#endif

	minutely_event.when.tv_sec = Now.tv_sec - (Now.tv_sec % 60) + 60;
	minutely_event.when.tv_usec = 0;
	add_event(&minutely_event);
}

void loadblink(struct event *event)
{
	static struct event loadblink_event = { .fn = loadblink };
	static int phase = 0;
	static long ld;
	static time_t last_ld = 0;

	led_tog(LOADLEDNUM);
	phase = !phase;

	if (Now.tv_sec > last_ld + 20)
	{
		last_ld = Now.tv_sec;
		ld = 100000000L / MAX(loadavg(), 1);
	}

	add_event_in(&loadblink_event, MIN(ld, phase ? 1000000 : 60000000));
}

int main(int argc, char **argv)
{
	init();
	init_leds();

	led_on(LOADLEDNUM);

	minutely(NULL);
	loadblink(NULL);
	schedule();
}
