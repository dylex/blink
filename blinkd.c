#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <stdarg.h>

#include "blink1.h"
#include "activity.h"
#include "notify.h"
#include "watch.h"
#include "loadavg.h"
#include "mail.h"
#include "command.h"

static int Blink1 = -1;

static void stop(int sig) __attribute__((noreturn));
static void stop(int sig) 
{
	if (Blink1 >= 0)
	{
		blink1_set(Blink1, 0, 0, 0);
		blink1_close(Blink1);
	}
	exit(sig == 0);
}

static void die(const char *msg, ...) __attribute__((format(printf, 1, 2), noreturn));
static void die(const char *msg, ...)
{
	va_list args;
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	stop(0);
}

int main(int argc, char **argv)
{
	if (signal(SIGTERM, &stop) == SIG_ERR ||
			signal(SIGINT, &stop) == SIG_ERR)
		die("signal: %m\n");

	Blink1 = blink1_open(NULL);
	if (Blink1 < 0)
		die("blink1_open: %m\n");

	const char *h = getenv("HOME");
	if (h && chdir(h) < 0)
		fprintf(stderr, "chdir(%s): %m\n", h);

	if (notify_init() < 0)
		die("notify_init: %m\n");
	loadavg_init();
	if (mail_init() < 0)
		fprintf(stderr, "mail_init: %m\n");
	if (command_init() < 0)
		fprintf(stderr, "command_init: %m\n");

	blink1_set(Blink1, 0, 0, 0);
	while (1)
	{
		interval_t d = active_run(Blink1);
		if (watch_run(d) < 0)
			die("watch_run: %m\n");
	}
}
