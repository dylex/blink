#ifndef ACTIVITY_H
#define ACTIVITY_H

#include <stdint.h>
#include <string.h>
#include "types.h"
#include "list.h"

static inline void color_cpy(color_t d, const color_t s)
	{ memcpy(d, s, sizeof(color_t)); }

static inline void color_set(color_t d, uint8_t v)
	{ memset(d, v, sizeof(color_t)); }

static inline int color_cmp(const color_t d, const color_t s)
{ 
	int x;
	if ((x = *(const uint16_t *)d-*(const uint16_t *)s))
		return x;
	return d[2]-s[2];
}

struct segment {
	color_t start, end;
	interval_t len;
};

struct activity;
typedef void activity_fn(struct activity *);
struct activity {
	struct segment seg;
	activity_fn *fun;

	interval_t rem;
	HLIST_NEXT(struct activity);
};

void activity_add(struct activity *);
void activity_rm(struct activity *);

void base_add(const color_t);
void base_rm(const color_t);

interval_t active_run(int);
void active_pop(interval_t);

#endif
