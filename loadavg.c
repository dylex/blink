#include <stdio.h>
#include <stdlib.h>
#include "activity.h"
#include "loadavg.h"

#define LOAD_FILE	"/proc/loadavg"
#define LOAD_UPDATE	(60*INTERVAL_SECOND)
#define LOAD_WHICH	1

loadavg_t Loadavg[3];

static loadavg_t load_read(char **s)
{
	unsigned long x;
	x = strtoul(*s, s, 10);
	if (**s != '.')
	{
		*s = NULL;
		return 0;
	}
	(*s)++;
	x *= 100;
	if (!(**s >= '0' && **s <= '9'))
		return x;
	x += 10*(*(*s)++-'0');
	if (!(**s >= '0' && **s <= '9'))
		return x;
	x += *(*s)++-'0';
	while (**s >= '0' && **s <= '9')
		(*s)++;
	return x;
}

static void load_update(struct activity *a, enum led led)
{
	FILE *f = fopen(LOAD_FILE, "r");
	if (f)
	{
		char buf[256];
		size_t r = fread(buf, 1, sizeof(buf)-1, f);
		fclose(f);
		buf[r] = 0;
		char *p = buf;
		unsigned i;
		for (i = 0; *p && i < sizeof(Loadavg)/sizeof(*Loadavg); i++)
			Loadavg[i] = load_read(&p);
	}
	activity_add(a, led);
}

static struct activity Load_update = { 
	{ .len = LOAD_UPDATE }, 
	.fun = &load_update 
};

static void load_blink(struct activity *a, enum led led)
{
	loadavg_t l = Loadavg[LOAD_WHICH];
	enum led led_start = a->led_start;
	a->led_start = a->led_end;
	a->led_end = led_start;
	if (l > 4*INTERVAL_SECOND*100/LOAD_UPDATE)
		a->seg.len = MAX(4*INTERVAL_SECOND*100/l, INTERVAL_SECOND/2);
	else
		a->seg.len = LOAD_UPDATE;
	activity_add(a, led);
}

static struct activity Load_blink = { 
	{ .len = LOAD_UPDATE }, 
	.led_start = LED_LOAD_1, 
	.led_end = LED_LOAD_0,
	.fun = &load_blink 
};

void loadavg_init()
{
	color_t c = { 0, COLOR_MAX/2, COLOR_MAX/2 };
	base_set(c, LED_LOAD_1);

	load_update(&Load_update, 0);
	load_blink(&Load_blink, 0);
}
