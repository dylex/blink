#ifndef ACTIVITY_H
#define ACTIVITY_H

#include <stdint.h>
#include <string.h>
#include "types.h"

typedef uint8_t color_t;
#define COLOR_MAX	((color_t)~0)
typedef color_t rgb_t[3];

#define for_rgb(c) for (c = 0; c < 3; c++)

static inline void rgbcpy(rgb_t d, const rgb_t s)
	{ memcpy(d, s, sizeof(rgb_t)); }

static inline void rgbset(rgb_t d, color_t v)
	{ memset(d, v, sizeof(rgb_t)); }

static inline int rgbcmp(const rgb_t d, const rgb_t s)
{ 
	int x;
	if ((x = *(const uint16_t *)d-*(const uint16_t *)s))
		return x;
	return d[2]-s[2];
}

struct segment {
	rgb_t start, end;
	interval_t len;
};

struct activity;
typedef void activity_fn(struct activity *);
struct activity {
	struct segment seg;
	activity_fn *fun;

	interval_t rem;
	struct activity *next;
};

void activity_add(struct activity *);

interval_t active_run(int);
void active_pop(interval_t);

#endif
