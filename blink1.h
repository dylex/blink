#ifndef BLINK1_H
#define BLINK1_H

#include <stdint.h>

int blink1_open(const char *);
void blink1_close(int);
int blink1_set(int, uint8_t r, uint8_t g, uint8_t b);
int blink1_fade(int, uint16_t t, uint8_t r, uint8_t g, uint8_t b);

#endif
