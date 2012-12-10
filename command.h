#ifndef COMMAND_H
#define COMMAND_H

#include "types.h"

#define COMMAND_SOCKET	"/tmp/.blink1.ctl"

enum command {
	COMMAND_NULL = 0,
	COMMAND_COLOR_SET,
	COMMAND_COLOR_ADD,
	COMMAND_COLOR_SUB
};

struct command_color {
	enum command cmd;
	color_t color;
};

int command_init();

#endif
