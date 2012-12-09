#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#include "blink1.h"
#include "activity.h"
#include "watch.h"
#include "loadavg.h"

static int Blink1 = -1;

int main(int argc, char **argv)
{
	Blink1 = blink1_open(NULL);
	if (Blink1 < 0)
	{
		fprintf(stderr, "blink1_open: %m\n");
		// return 1;
	}

	notify_init();
	loadavg_init();

	blink1_set(Blink1, 0, 0, 0);
	while (1)
	{
		struct timeval t;
		gettimeofday(&t, NULL);
		printf("%lu.%06lu\n", t.tv_sec, t.tv_usec);

		interval_t d = active_run(Blink1);
		if (watch_run(d) < 0)
		{
			fprintf(stderr, "watch_run: %m\n");
			return 3;
		}
	}
}
