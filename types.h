#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>

typedef uint16_t interval_t;
#define INTERVAL_SECOND	((interval_t)100)

#define COLOR_MAX	255
#define COLOR_COUNT	3
typedef uint8_t color_t[COLOR_COUNT];
#define for_color(c) for (c = 0; c < COLOR_COUNT; c++)

#endif
