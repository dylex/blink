#ifndef CONTROL_H
#define CONTROL_H

#include "types.h"

#define CONTROL_SOCKET	"/tmp/.blink1.ctl"

enum cmd {
	CMD_NULL = 0,
	CMD_SET,
	CMD_ADD,
	CMD_SUB
};

struct cmd_color {
	enum cmd cmd;
	color_t color;
};

int control_init();

#endif
