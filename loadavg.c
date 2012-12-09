#include <stdio.h>
#include <stdlib.h>
#include "activity.h"
#include "loadavg.h"

#define LOAD_FILE	"/proc/loadavg"
#define LOAD_UPDATE	(60*INTERVAL_SECOND)
#define LOAD_WHICH	0
#define LOAD_LOW	0
#define LOAD_HIGH	1000
#define LOAD_COLOR	{ 0, 0, COLOR_MAX/2 }

loadavg_t Loadavg[3];
static const rgb_t Load_color = LOAD_COLOR;

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

static void load_update(struct activity *a)
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
	activity_add(a);
}

static struct activity Load_update = { { .len = LOAD_UPDATE }, .fun = &load_update };

static void load_blink(struct activity *a)
{
	loadavg_t l = Loadavg[LOAD_WHICH];
	a->seg.len = Load_update.rem+1;
	rgbcpy(a->seg.start, a->seg.end);
	if (l < LOAD_LOW)
		rgbset(a->seg.end, 0);
	else if (l > LOAD_HIGH)
		rgbcpy(a->seg.end, Load_color);
	else
	{
		if (rgbcmp(a->seg.start, Load_color))
			rgbcpy(a->seg.end, Load_color);
		else
			rgbset(a->seg.end, 0);
		if (l > 4*INTERVAL_SECOND*100/LOAD_UPDATE)
			a->seg.len = 4*INTERVAL_SECOND*100/l;
	}
	activity_add(a);
}

static struct activity Load_blink = { { .len = LOAD_UPDATE }, .fun = &load_blink };

void loadavg_init()
{
	load_update(&Load_update);
	load_blink(&Load_blink);
}
