#include <argp.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <math.h>

#include "command.h"

static int Command_sock = -1;

static int parse_interval(interval_t *i, const char *s)
{
	char *e = NULL;
	double x = strtod(s, &e);
	if (!e || x < 0)
		return -1;
	if (e == s)
		x = 1;
	switch (*e)
	{
		case 's':
			e++;
			break;
			
		case 'm':
			e++;
			x *= 60;
			break;
	}
	*i = ceil(INTERVAL_SECOND*x);
	return e - s;
}

static int8_t hex1(char c)
{
	switch (c)
	{
		case '0' ... '9': return c - '0';
		case 'a' ... 'f': return 10 + c - 'a';
		case 'A' ... 'F': return 10 + c - 'A';
		default: return -1;
	}
}

static int parse_hex_color(color_t c, const char *s)
{
	if (*s == '#')
		s ++;
	uint8_t h[2*COLOR_COUNT];
	unsigned i = 0;
	while (i < 2*COLOR_COUNT)
	{
		int8_t x = hex1(s[i]);
		if (x < 0)
			break;
		h[i++] = x;
	}
	if (i >= 2*COLOR_COUNT)
	{
		for_color (i)
			c[i] = h[2*i] << 4 | h[2*i+1];
		return 2*COLOR_COUNT;
	}
	if (i >= COLOR_COUNT)
	{
		for_color (i)
			c[i] = h[i] << 4 | h[i];
		return COLOR_COUNT;
	}
	return -1;
}

struct token {
	enum {
		PARSE_NULL = 0,
		PARSE_COLOR,
		PARSE_INTERVAL,
		PARSE_BREAK
	} t;
	union {
		color_t c;
		interval_t i;
	};
};

static int parse_token(struct token *r, const char *s, struct argp_state *state)
{
	const char *p = s;
	while (isspace(*p))
		p++;
	switch (*p)
	{
		case 0:
			r->t = PARSE_NULL;
			return p - s;

		case ',':
		case ';':
			r->t = PARSE_BREAK;
			return p - s + 1;

		case '#':
		hex_color: {
			r->t = PARSE_COLOR;
			int l = parse_hex_color(r->c, p);
			if (l <= 0)
			{
				if (state)
					argp_error(state, "invalid hex color: %s", s);
				return l;
			}
			return p - s + l;
		}

		interval: {
			r->t = PARSE_INTERVAL;
			int l = parse_interval(&r->i, p);
			if (l <= 0)
			{
				if (state)
					argp_error(state, "invalid interval: %s", s);
				return l;
			}
			return p - s + l;
		}

		color: goto hex_color;
	}

	size_t l = strspn(p, "0123456789");
	switch (p[l])
	{
		case 'a' ... 'f':
		case 'A' ... 'F':
			goto hex_color;
		case '.':
		case 's':
		case 'm':
			goto interval;
	}
	switch (r->t)
	{
		case PARSE_COLOR:    goto color;
		case PARSE_INTERVAL: goto interval;
		default: break;
	}
	if (l == COLOR_COUNT || l == 2*COLOR_COUNT)
		goto hex_color;
	else
		goto interval;
}

static int process_args(int an, char **av, struct argp_state *state)
{
	int ai;
	enum {
		START,
		LEN,
		END,
		NEXT
	} s = START;
	struct command_sequence cmd = { COMMAND_SEQUENCE };
	unsigned si = 0;
	color_t c = {};

	for (ai = 0; ai < an; ai ++)
	{
		const char *p = av[ai];
		while (*p) {
			struct token t;
			switch (s)
			{
				case START:
				case END:
					t.t = PARSE_COLOR;
					break;
				case LEN:
					t.t = PARSE_INTERVAL;
					break;
				default:
					t.t = PARSE_NULL;
			}

			int r = parse_token(&t, p, state);
			if (r <= 0)
				break;
			p += r;
			if (t.t == PARSE_COLOR)
				color_cpy(c, t.c);

			switch (s)
			{
				case END:
					color_cpy(cmd.seq[si].end, c);
					if (t.t == PARSE_COLOR)
					{
						s = NEXT;
						continue;
					}

				case NEXT:
					si ++;
					if (t.t == PARSE_BREAK)
					{
						s = START;
						continue;
					}

				case START:
					if (si >= CMD_SEQ_MAX_SEG)
						argp_error(state, "sequence too long");
					color_cpy(cmd.seq[si].start, c);
					if (t.t == PARSE_COLOR)
					{
						s = LEN;
						continue;
					}

				case LEN:
					if (t.t == PARSE_INTERVAL)
					{
						cmd.seq[si].len = t.i;
						s = END;
						continue;
					}
			}
			argp_error(state, "unexpected: %s", p);
		}
	}
	if (s < END)
		argp_error(state, "incomplete sequence specification");
	if (s == END)
		color_cpy(cmd.seq[si].end, c);
	si ++;

	if (send(Command_sock, &cmd, (char *)&cmd.seq[si] - (char *)&cmd, 0) < 0)
		argp_failure(state, 3, errno, "send command");

	return ai;
}

static const struct argp_option Options[] = 
	{ { "mask", 'm', "COLOR", 0, "mask set color by COLOR [FFF]" }
	, { "set", 's', "COLOR", 0, "set COLOR & mask" }
	, { "on", 'i', "COLOR", 0, "equivalent to -m COLOR -s FFF" }
	, { "off", 'o', "COLOR", 0, "equivalent to -m COLOR -s 000" }
	, {}
	};

static error_t process(int key, char *optarg, struct argp_state *state)
{
	static struct command_color_mask cmd = { .mask = { COLOR_MAX, COLOR_MAX, COLOR_MAX } };
	enum color c;
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

		case 'i':
		case 'o':
			for_color (c)
				cmd.color[c] = (key == 'i' ? COLOR_MAX : 0);
		case 'm':
			if (parse_hex_color(cmd.mask, optarg) <= 0)
				argp_error(state, "invalid color: %s", optarg);
			if (key == 'm')
				break;

		case 's':
			cmd.cmd = COMMAND_COLOR_SET;
			if (key == 's' && parse_hex_color(cmd.color, optarg) <= 0)
				argp_error(state, "invalid color: %s", optarg);
			if (send(Command_sock, &cmd, sizeof(struct command_color_mask), 0) < 0)
				argp_failure(state, 3, errno, "send command");
			break;

		case ARGP_KEY_ARG:
			switch (optarg[0])
			{
				case '=': cmd.cmd = COMMAND_COLOR_SET; break;
				case '+': cmd.cmd = COMMAND_COLOR_ADD; break;
				case '-':
				case '_': cmd.cmd = COMMAND_COLOR_SUB; break;
				default: return ARGP_ERR_UNKNOWN;
			}
			if (parse_hex_color(cmd.color, &optarg[1]) <= 0)
				argp_error(state, "invalid color: %s", &optarg[1]);
			if (send(Command_sock, &cmd, sizeof(struct command_color), 0) < 0)
				argp_failure(state, 3, errno, "send command");
			break;

		case ARGP_KEY_ARGS: {
			int r = process_args(state->argc - state->next, state->argv + state->next, state);
			if (r <= 0)
				return ARGP_ERR_UNKNOWN;
			state->next += r;
		}	break;

		default:
			return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static const struct argp argp_parser = {
	.options = Options,
	.parser = &process,
	.args_doc = "[[=+-]COLOR] [SEGMENT [[,] ...]]",
	.doc = "Send commands to blink1d\v"
		"  [=+-]COLOR: set, add, or remove the fixed COLOR\n"
		"  SEGMENT: part of a sequence of color changes\n"
		"    COLOR LEN: turn on COLOR for LEN\n"
		"    COLOR LEN COLOR: fade from COLOR to COLOR in LEN\n"
		"    LEN COLOR: fade from previous color to COLOR\n"
		"  COLOR: an [#]RGB or [#]RRGGBB hex triplet\n"
		"  LEN: SECONDS[s] or MINUTESm\n"
};

int main(int argc, char **argv)
{
	return argp_parse(&argp_parser, argc, argv, ARGP_IN_ORDER, 0, 0);
}
