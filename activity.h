#ifndef ACTIVITY_H
#define ACTIVITY_H

#include <stdbool.h>
#include "types.h"
#include "list.h"

void active_blink(enum led, int blink);

struct activity;
typedef void activity_fn(struct activity *, enum led);
struct activity {
	struct segment seg;
	activity_fn *fun;
	enum led led_start : 4;
	enum led led_end : 4;

	interval_t rem;
	HLIST_NEXT(struct activity);
};

void activity_add(struct activity *, enum led);
void activity_rm(struct activity *, enum led, color_t);
bool activity_active(const struct activity *);

struct activity_then {
	struct activity act;
	struct activity *then;
};

void base_set(const color_t, enum led);
void base_add(const color_t, enum led);
void base_rm(const color_t, enum led);

interval_t active_run(void);
void active_pop(interval_t);

#define activity_free ((activity_fn *)&free)
activity_fn activity_then;
activity_fn activity_then_free;

#endif
