#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "types.h"
#include "list.h"

struct activity;
typedef void activity_fn(struct activity *);
struct activity {
	struct segment seg;
	activity_fn *fun;

	interval_t rem;
	HLIST_NEXT(struct activity);
};

void activity_add(struct activity *);
void activity_rm(struct activity *, color_t);

struct activity_then {
	struct activity act;
	struct activity *then;
};

void base_add(const color_t);
void base_rm(const color_t);

interval_t active_run(int);
void active_pop(interval_t);

#define activity_free ((activity_fn *)&free)
activity_fn activity_then;
activity_fn activity_then_free;

#endif
