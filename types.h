#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <string.h>

#define MIN(X, Y) ({ \
		typeof(X) _x = (X); \
		typeof(Y) _y = (Y); \
		_x < _y ? _x : _y; \
	})
#define MAX(X, Y) ({ \
		typeof(X) _x = (X); \
		typeof(Y) _y = (Y); \
		_x > _y ? _x : _y; \
	})

typedef uint16_t interval_t;
#define INTERVAL_INF	((interval_t)-1)
#define INTERVAL_SECOND	((interval_t)100)

#define COLOR_MAX	255
enum color {
	RED = 0,
	GREEN,
	BLUE,
	COLOR_COUNT
};
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

struct segment {
	color_t start, end;
	interval_t len;
};

enum led {
	LED_NONE = 0,
	LED_1,
	LED_2,
	LED_LOAD_0,
	LED_LOAD_1,
	LED_COUNT
#define LED_MAX	LED_COUNT
};
#define for_led(l) for (l = LED_MAX-1; (int)l >= 0; l--)

#endif
