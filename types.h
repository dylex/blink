#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <string.h>

typedef uint16_t interval_t;
#define INTERVAL_SECOND	((interval_t)100)

#define COLOR_MAX	255
#define COLOR_COUNT	3
typedef uint8_t color_t[COLOR_COUNT];
#define for_color(c) for (c = 0; c < COLOR_COUNT; c++)

static inline void color_cpy(color_t d, const color_t s)
	{ memcpy(d, s, sizeof(color_t)); }

static inline int color_cmp(const color_t d, const color_t s)
{ 
	int x;
	if ((x = *(const uint16_t *)d-*(const uint16_t *)s))
		return x;
	return d[2]-s[2];
}

static const color_t color_zero;

#endif
