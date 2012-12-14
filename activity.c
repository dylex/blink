#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "blink1.h"
#include "activity.h"

typedef int lcolor_t[COLOR_COUNT];

static HLIST_HEAD(struct activity) Active;
static lcolor_t Base;

static void l_color(color_t o, const lcolor_t i)
{
	unsigned c;
	for_color (c) {
		if (i[c] > COLOR_MAX)
			o[c] = COLOR_MAX;
		else if (i[c] < 0)
			o[c] = 0;
		else
			o[c] = i[c];
	}
}

static inline int interp(int s, interval_t l, int e, interval_t r)
{
	assert(r <= l);
	if (!l)
		return s;
	return e + (s-e)*r/l;
}

static inline void color_add(lcolor_t o, const color_t i)
{
	unsigned c;
	for_color (c)
		o[c] += i[c];
}

static inline void color_sub(lcolor_t o, const color_t i)
{
	unsigned c;
	for_color (c)
		if ((o[c] -= i[c]) < 0)
			o[c] = 0;
}

static void segment_interp(color_t o, const struct segment *s, interval_t r)
{
	unsigned c;
	for_color (c)
		o[c] = interp(s->start[c], s->len, s->end[c], r);
}

static void segment_interp_add(lcolor_t o, const struct segment *s, interval_t r)
{
	color_t t;
	segment_interp(t, s, r);
	color_add(o, t);
}

static struct segment Current;

static void segment_run(int blink, const struct segment *s)
{
	if (color_cmp(Current.start, s->start))
		blink1_set(blink, s->start[0], s->start[1], s->start[2]);
	if (s->len && color_cmp(s->end, s->start))
		blink1_fade(blink, s->len, s->end[0], s->end[1], s->end[2]);
	Current = *s;
}

void activity_add(struct activity *a)
{
	struct activity **p;
	assert(!a->next);
	a->rem = a->seg.len;
	for_hlist_p (p, Active)
	{
		if ((*p)->rem > a->rem)
			break;
		a->rem -= (*p)->rem;
	}
	if (*p)
		(*p)->rem -= a->rem;
	hlist_ins(a, *p);
}

void activity_rm(struct activity *a)
{
	if (a->next)
		a->next->rem += a->rem;
	a->rem = 0;
	hlist_del(a);
}

void base_add(const color_t c)
{
	color_add(Base, c);
}

void base_rm(const color_t c)
{
	color_sub(Base, c);
}

static void active_segment(struct segment *o)
{
	lcolor_t start, end;
	memcpy(start, Base, sizeof(lcolor_t));
	memcpy(end, Base, sizeof(lcolor_t));
	struct activity *a = Active;
	if (a)
		o->len = a->rem;
	interval_t t = 0;
	while (a)
	{
		t += a->rem;
		segment_interp_add(start, &a->seg, t);
		segment_interp_add(end, &a->seg, t-o->len);
		a = a->next;
	}
	l_color(o->start, start);
	l_color(o->end, end);
}

static void activity_done(struct activity *a)
{
	hlist_del(a);
	if (a->fun)
		a->fun(a);
}

interval_t active_run(int blink)
{
	struct segment s = { .len = INTERVAL_INF };
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
	struct activity *a = Active;
	if (!a)
		return;
	assert(t <= a->rem);
	if ((a->rem -= t))
		return;
	activity_done(a);
}

void activity_then(struct activity *a)
{
	struct activity_then *at = (struct activity_then *)a;
	activity_add(at->then);
}

void activity_then_free(struct activity *a)
{
	activity_then(a);
	free(a);
}
