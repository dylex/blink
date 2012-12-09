#include <string.h>
#include <assert.h>

#include "blink1.h"
#include "activity.h"

#define for_activity(A, LIST) for (A = (LIST); A; A = A->next)
#define for_activity_p(AP, LIST) for (AP = &(LIST); *AP; AP = &(*AP)->next)

static struct activity *Active_fixed, *Active_timed;

static inline color_t color_add(color_t x, color_t y)
{
	unsigned z = x + y;
	return z > COLOR_MAX ? COLOR_MAX : z;
}

static inline color_t color_interp(color_t s, interval_t l, color_t e, interval_t r)
{
	assert(l && r <= l);
	signed d = s - e;
	return e + d*r/l;
}

static void rgb_add(rgb_t o, const rgb_t i)
{
	unsigned c;
	for_rgb (c)
		o[c] = color_add(o[c], i[c]);
}

static void segment_interp(rgb_t o, const struct segment *s, interval_t r)
{
	unsigned c;
	for_rgb (c)
		o[c] = color_add(o[c], color_interp(s->start[c], s->len, s->end[c], r));
}

static struct segment Current;

static void segment_run(int blink, const struct segment *s)
{
	if (rgbcmp(Current.start, s->start))
		blink1_set(blink, s->start[0], s->start[1], s->start[2]);
	if (s->len && rgbcmp(s->end, s->start))
		blink1_fade(blink, s->len, s->end[0], s->end[1], s->end[2]);
	Current = *s;
}

void activity_add(struct activity *a)
{
	assert(!a->next);
	if (a->seg.len)
	{
		struct activity **p;
		a->rem = a->seg.len;
		for_activity_p (p, Active_timed)
		{
			if ((*p)->rem > a->rem)
				break;
			a->rem -= (*p)->rem;
		}
		if ((a->next = *p))
			a->next->rem -= a->rem;
		*p = a;
	}
	else
	{
		a->rem = 0;
		a->next = Active_fixed;
		Active_fixed = a;
	}
}

static void active_segment(struct segment *o)
{
	struct activity *a;
	for_activity (a, Active_fixed)
	{
		rgb_add(o->start, a->seg.start);
		rgb_add(o->end, a->seg.end);
	}
	if (!Active_timed)
		return;
	o->len = Active_timed->rem;
	interval_t t = 0;
	for_activity (a, Active_timed)
	{
		t += a->rem;
		segment_interp(o->start, &a->seg, t);
		segment_interp(o->end, &a->seg, t-o->len);
	}
}

static void activity_done(struct activity *a)
{
	if (a->fun)
		a->fun(a);
}

interval_t active_run(int blink)
{
	struct segment s = {};
	active_segment(&s);
	segment_run(blink, &s);
	return s.len;
}

void active_pop(interval_t t)
{
	if (Current.len)
	{
		segment_interp(Current.start, &Current, Current.len-t);
		Current.len -= t;
	}
	struct activity *a = Active_timed;
	if (!a)
		return;
	assert(t <= a->rem);
	if ((a->rem -= t))
		return;
	Active_timed = a->next;
	a->next = NULL;
	activity_done(a);
}
