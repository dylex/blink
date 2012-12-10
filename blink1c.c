#include <argp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include "command.h"

static int Command_sock = -1;

static const struct argp_option options[] = {
	{"set",	's', "[=+-]COLOR", 0, 		"set, add, or remove current color", 1},

	{} 
};

static int parse_color(color_t c, const char *s)
{
	if (sscanf(s, "#%02hhx%02hhx%02hhx ", &c[0], &c[1], &c[2]) == 3)
		return 1;
	if (sscanf(s, "%hhd,%hhd,%hhd ", &c[0], &c[1], &c[2]) == 3)
		return 1;
	if (!strcmp(s, "0"))
	{
		color_cpy(c, color_zero);
		return 1;
	}
	return 0;
}

error_t parse(int key, char *optarg, struct argp_state *state)
{
	switch (key)
	{
		case ARGP_KEY_INIT: {
			struct sockaddr_un sa = { AF_UNIX, COMMAND_SOCKET };
			if ((Command_sock = socket(PF_UNIX, SOCK_DGRAM, 0)) < 0 ||
					connect(Command_sock, (struct sockaddr *)&sa, SUN_LEN(&sa)) < 0)
				argp_failure(state, 3, errno, "connect command socket");
		}	break;

		case ARGP_KEY_FINI:
			close(Command_sock);
			break;

		case 's': {
			struct command_color cmd;
			switch (optarg[0])
			{
				case '=': cmd.cmd = COMMAND_COLOR_SET; break;
				case '+': cmd.cmd = COMMAND_COLOR_ADD; break;
				case '-': cmd.cmd = COMMAND_COLOR_SUB; break;
				default: argp_error(state, "Set color must start with one of '=', '+', or '-'");
			}
			if (parse_color(cmd.color, &optarg[1]) < 1)
				argp_error(state, "Invalid color: %s", &optarg[1]);
			if (send(Command_sock, &cmd, sizeof(cmd), 0) < 0)
				argp_failure(state, 3, errno, "send command");
		}	break;

		default:
			return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static const struct argp argp_parser = {
	.options = options,
	.parser = &parse
};

int main(int argc, char **argv)
{
	return argp_parse(&argp_parser, argc, argv, ARGP_IN_ORDER, 0, 0);
}
