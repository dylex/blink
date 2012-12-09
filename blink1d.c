#include <unistd.h>
#include <stdio.h>
#include <time.h>

#include "blink1.h"
#include "activity.h"
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

	loadavg_init();

	while (1)
	{
		interval_t d = active_run(Blink1);
		if (d)
		{
			struct timespec ts = { d / INTERVAL_SECOND, d % INTERVAL_SECOND * (1000000000L / INTERVAL_SECOND) };
			if (nanosleep(&ts, &ts) < 0)
				d -= ts.tv_sec * INTERVAL_SECOND + ts.tv_nsec / (1000000000L / INTERVAL_SECOND);
			active_pop(d);
		}
		else
		{
			pause();
		}
	}
}
