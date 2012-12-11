#ifndef COMMAND_H
#define COMMAND_H

#include "types.h"

#define COMMAND_SOCKET	"/tmp/.blink1.ctl"

enum command {
	COMMAND_NULL = 0,
	COMMAND_COLOR_SET,
	COMMAND_COLOR_ADD,
	COMMAND_COLOR_SUB,
	COMMAND_SEQUENCE
};

struct command_color {
	enum command cmd;
	color_t color;
};

#define CMD_SEQ_MAX_SEG 16
struct command_sequence {
	enum command cmd;
	struct segment seq[CMD_SEQ_MAX_SEG];
};

int command_init();

#endif
