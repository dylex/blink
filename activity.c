#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include "blink1.h"
#include "main.h"
#include "activity.h"

typedef int lcolor_t[COLOR_COUNT];

static struct led {
	HLIST_NEXT(struct led);
	HLIST_HEAD(struct activity) active;
	lcolor_t base;
	void update(const struct led *this, const struct segment *);
	struct segment current;
};

static struct state {
	int blink;
	HLIST_HEAD(struct activity) active;
	lcolor_t base;
	struct segment current;
} State[LED_MAX];

void active_blink(enum led led, int blink)
{
	assert(blink != 0);
	State[led].blink = blink;
	if (blink > 0)
		blink1_set(blink, 0, 0, 0);
}

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

static inline void color_set(lcolor_t o, const color_t i)
{
	unsigned c;
	for_color (c)
		o[c] = i[c];
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

static inline int interp(int s, interval_t l, int e, interval_t r)
{
	if (!l)
		return s;
	return e + (s-e)*r/l;
}

static void segment_interp(color_t o, const struct segment *s, interval_t r)
{
	assert(r <= s->len);
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

static void segment_run(enum led led, const struct segment *s)
{
	if (!led)
		return;
	if (color_cmp(State[led].current.start, s->start))
	{
		if (Debug) {
			printf("was    %02X%02X%02X [%d]\n", State[led].current.start[0], State[led].current.start[1], State[led].current.start[2], led);
			printf("       %02X%02X%02X [%d]\n", s->start[0], s->start[1], s->start[2], led);
		}
		if (State[led].blink > 0)
			blink1_set(State[led].blink, s->start[0], s->start[1], s->start[2]);
	}
	if (s->len && color_cmp(s->end, s->start))
	{
		if (Debug)
			printf("%5u->%02X%02X%02X [%d]\n", s->len, s->end[0], s->end[1], s->end[2], led);
		if (State[led].blink > 0)
			blink1_fade(State[led].blink, s->len, s->end[0], s->end[1], s->end[2]);
	}
	State[led].current = *s;
}

static bool activity_empty(const struct activity *a) {
	return !a->led || !color_cmp(a->seg.start, color_zero) && !color_cmp(a->seg.end, color_zero) && !a->led_start && !a->led_end;
}

static interval_t activity_rem(struct activity *act, enum led led)
{
	interval_t t = 0;
	struct activity *a;
	for_hlist (a, State[led].active)
	{
		t += a->rem;
		if (a == act)
			return t;
	}
	return INTERVAL_INF;
}

static enum led activity_led(struct activity *a) {
	assert(a->led < LED_MAX);
	return activity_empty(a) ? LED_NONE : a->led;
}

void activity_add(struct activity *a)
{
	enum led led = activity_led(a);
	if (a->led_start)
		assert(a->led_start > led && a->led_start < LED_MAX);
	if (a->led_end)
		assert(a->led_end > led && a->led_end < LED_MAX);
	struct activity **p;
	assert(!a->next);
	a->rem = a->seg.len;
	for_hlist_p (p, State[led].active)
	{
		if ((*p)->rem > a->rem)
			break;
		a->rem -= (*p)->rem;
	}
	if (*p)
		(*p)->rem -= a->rem;
	hlist_ins(a, *p);
}

void activity_rm(struct activity *a, color_t c)
{
	enum led led = activity_led(a);
	if (c)
		segment_interp(c, &a->seg, activity_rem(a, led));
	if (a->next)
		a->next->rem += a->rem;
	a->rem = 0;
	hlist_del(a);
}

bool activity_active(const struct activity *a)
{
	return hlist_on_list(a);
}

void base_set(const color_t c, enum led led)
{
	color_set(State[led].base, c);
}

void base_add(const color_t c, enum led led)
{
	color_add(State[led].base, c);
}

void base_rm(const color_t c, enum led led)
{
	color_sub(State[led].base, c);
}

static void active_segment(const struct state *state, struct segment *o)
{
	lcolor_t start, end;
	interval_t t;
	struct activity *a;

	o->len = state->active ? state->active->rem : INTERVAL_INF;
shorten:
	memcpy(start, state->base, sizeof(lcolor_t));
	memcpy(end, state->base, sizeof(lcolor_t));
	t = 0;
	for (a = state->active; a; a = a->next)
	{
		t += a->rem;
		if (a->led_start)
			memcpy(a->seg.start, State[a->led_start].current.start, sizeof(color_t));
		if (a->led_end)
		{
			struct segment *e = &State[a->led_end].current;
			if (e->len && e->len < o->len)
			{
				o->len = e->len;
				goto shorten;
			}
			segment_interp(a->seg.end, e, e->len-o->len);
		}
		segment_interp_add(start, &a->seg, t);
		segment_interp_add(end, &a->seg, t-o->len);
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

interval_t active_run()
{
	enum led led;
	interval_t t = INTERVAL_INF;
	for_led (led)
	{
		struct segment s;
		active_segment(&State[led], &s);
		segment_run(led, &s);
		if (State[led].active)
			t = MIN(t, State[led].active->rem);
	}
	return t;
}

void active_pop(interval_t t)
{
	if (Debug)
		printf("%5u\n", t);
	enum led led;
	for_led (led)
	{
		struct segment *current = &State[led].current;
		if (current->len)
		{
			segment_interp(current->start, current, current->len-t);
			current->len -= t;
		}
		struct activity *a = State[led].active;
		if (a)
		{
			assert(t <= a->rem);
			a->rem -= t;
		}
	}
	for_led (led)
	{
		struct activity *a;
		while ((a = State[led].active) && !a->rem)
			activity_done(a);
	}
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
