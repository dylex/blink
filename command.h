#ifndef COMMAND_H
#define COMMAND_H

#include "types.h"

#define COMMAND_SOCKET	"/tmp/.blink.ctl"

enum command {
	COMMAND_NULL = 0,
	COMMAND_COLOR_SET,
	COMMAND_COLOR_ADD,
	COMMAND_COLOR_SUB,
	COMMAND_SEQUENCE
};

struct command_color {
	enum command cmd;
	enum led led;
	color_t color;
};

struct command_color_mask {
	enum command cmd;
	enum led led;
	color_t color, mask;
};

#define CMD_SEQ_MAX_SEG 16
struct command_sequence {
	enum command cmd;
	enum led led;
	struct segment seq[CMD_SEQ_MAX_SEG];
};

int command_init();

#endif
