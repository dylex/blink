#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/ioctl.h>
#include <argp.h>
#include "ppldd.h"

#ifdef PPLDD_ENABLE_LCD
#define MAX_DATALEN	(4*(PPLDD_LCD_ROWS*PPLDD_LCD_COLS+4))	/* Plenty of space */
#endif
static const unsigned char revnibble[16] = { 0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15 };

void die(char *);
int parse_leds(const char *);
int parse_led(const char *);
#ifdef PPLDD_ENABLE_LCD
void lcd_send_wrap(struct ppldd_lcd_data *);
void write_lcd(const char *, int);
#endif
void print_status(void);

/******************************************************************** ARGP PARSING */

static const struct argp_option argp_options[] = {
/*	{"verbose",	'v', 0, 0, 		"print all ioctls", -1}, */
	{"print",	'p', 0, 0, 		"print current display (cat /dev/ppldd)", -1},
	{"unuse",	'u', 0, OPTION_HIDDEN, 	"dec module use count (yuck!)", -1},

	{"get-leds",	'g', "BASE", OPTION_ARG_OPTIONAL, "print led state (BASE=[bin],hex)", 1},
	{"set-leds",	's', "LEDS", 0,		"set leds to LEDS (num,bin,hex)", 1},
	{"off-led",	'0', "LED", 0, 		"turn off LED", 2},
	{"on-led",	'1', "LED", 0, 		"turn on LED", 2},
	{"toggle-led",	't', "LED", 0, 		"toggle LED", 2},

#ifdef PPLDD_ENABLE_LCD
	{"clear-lcd",	'z', 0, 0, 		"clear the lcd display", 4},
	{"reset-lcd",	0, 0, OPTION_ALIAS }, 
	{"row",		'y', "ROW", 0, 		"set lcd position to ROW [0]", 5},
	{"column",	'x', "COL", 0, 		"set lcd position to COL [0]", 5},
	{"wrap-lcd",	'w', 0, 0, 		"enable wrapping if text overflows", 5},
	{"character",	'c', "CHR", 0, 		"write CHR (int) to lcd", 6},
	{"message",	'm', "TEXT", 0, 	"write TEXT to lcd (same as arguments)", 6},
	{"string",	0, 0, OPTION_ALIAS },
	{"file",	'f', "FILE", OPTION_ARG_OPTIONAL, "write text from FILE [stdin] to lcd", 6},
#endif

	{} 
};
error_t parse(int, char *, struct argp_state *);

static const struct argp argp_parser = {
	options:	argp_options,
	parser:		&parse,
	args_doc:	
#ifdef PPLDD_ENABLE_LCD
		"[LCD TEXT]",
#else
		"",
#endif
	doc:		"Control the ppldd device.\vArgument actions happen sequentially, so order matters."
};
	
/******************************************************************** DATA */

const char *progname;
int ppldd_dev;
#ifdef PPLDD_ENABLE_LCD
int lcd_row = 0, lcd_col = 0;
int opt_wrap = 0;
#endif

/******************************************************************** FUNCTIONS */

void die(char *err)
{
	fprintf(stderr, "%s: %s: %m\n", progname, err);
	exit(1);
}

int parse_leds(const char *p)
{
	ppldd_led_stat_t leds;
	char *c;
	int i;

	/* if (((p[0] >= '0' && p[0] <= '8') || (p[0] >= 'a' XXX )) && !p[1])
		return 1 << (p[0] - '1'); */
	
	leds = strtol(p, &c, 2);
	if (!*c) return leds;
	leds = strtol(p, &c, 16);
	if (!*c) return leds;

	fprintf(stderr, "%s: unknown leds format: %s\n", progname, p);
	return -1;
}


int parse_led(const char *p)
{
	int i = atoi(p);
	if (i <= 0 || i > PPLDD_LED_COUNT)
	{
		fprintf(stderr, "%s: led %d out of range (1..%d)\n", progname, i, PPLDD_LED_COUNT);
		return -1;
	}
	return i;
}

#ifdef PPLDD_ENABLE_LCD
void lcd_send_wrap(struct ppldd_lcd_data *cmd)
{
	if (ioctl(ppldd_dev, PPLDD_IOC_LCD_WRITE, cmd) == -1)
		die("ioctl LCD_WRITE");
	cmd->col = lcd_col = 0;
	if (++lcd_row >= PPLDD_LCD_ROWS)
	{
		if (opt_wrap)
			lcd_row = 0;
		else
			fprintf(stderr, "%s: extra rows for lcd ignored\n", progname);
	}
	cmd->row = lcd_row;
	cmd->len = 0;
}

void write_lcd(const char *data, int datalen)
{
	struct ppldd_lcd_data cmd;
	int i;

	cmd.row = lcd_row;
	cmd.col = lcd_col;
	cmd.len = 0;
	for (i = 0; i < datalen; i++)
	{
		if (lcd_row >= PPLDD_LCD_ROWS)
		{
			/* out of rows */
		}
		else if (data[i] == '\n')
		{
			memset(&cmd.data[cmd.len], PPLDD_LCD_CHR_BLANK, PPLDD_LCD_COLS-lcd_col); /* fill rest */
			cmd.len += PPLDD_LCD_COLS-lcd_col; /* = PPLDD_LCD_COLS - cmd.col; */
			lcd_send_wrap(&cmd);
		}
		else if (lcd_col >= PPLDD_LCD_COLS)
		{
			/* out of columns */
		}
		else
		{
			cmd.data[cmd.len++] = data[i];
			if (++lcd_col >= PPLDD_LCD_COLS)
			{
				if (opt_wrap)
					lcd_send_wrap(&cmd);
				else
					fprintf(stderr, "%s: extra characters for lcd row %d ignored\n", progname, lcd_row);
			}
		}
	}
	if (cmd.len)
		lcd_send_wrap(&cmd);
	lcd_col = lcd_row = 0; /* reset */
}
#endif

void print_status()
{
	char buf[PPLDD_INFO_SIZE+8];
	int l;

	lseek(ppldd_dev, 0, SEEK_SET);
	if ((l = read(ppldd_dev, buf, PPLDD_INFO_SIZE+7)) == -1)
		die("read ppldd");
	if (l < PPLDD_INFO_SIZE || buf[PPLDD_LED_COUNT] != '\n')
	{
		printf("Can't parse status output:\n");
		write(STDOUT_FILENO, buf, l);
	}
	else
	{
		buf[PPLDD_LED_COUNT] = 0;
		printf("LEDS: %s\n", buf);
#ifdef PPLDD_ENABLE_LCD
		printf("LCD:\n");
		write(STDOUT_FILENO, buf+PPLDD_LED_COUNT+1, l-PPLDD_LED_COUNT-1);
#endif
	}
}

/******************************************************************** MAIN */

error_t parse(int key, char *optarg, struct argp_state *state)
{
	int x; /* just a random int hanging around */

	switch(key) {
		case ARGP_KEY_INIT:
			if ((ppldd_dev = open("/dev/" PPLDD_DEVICE, O_RDONLY)) == -1)
				die("open PPLDD_DEVICE");
			break;

		case ARGP_KEY_FINI:
			close(ppldd_dev);
			break;

		case 'p': /* print */
			print_status();
			break;

		case 'u': /* unuse */
			if (ioctl(ppldd_dev, PPLDD_IOC_UNUSE) == -1)
				die("ioctl UNUSE");
			break;

		/********************** LED */
		case '0': /* off-led */
			if ((x = parse_led(optarg)) != -1)
				if (ioctl(ppldd_dev, PPLDD_IOC_LED_OFF, x) == -1)
					die("ioctl LED_OFF");
			break;

		case '1': /* on-led */
			if ((x = parse_led(optarg)) != -1)
				if (ioctl(ppldd_dev, PPLDD_IOC_LED_ON, x) == -1)
					die("ioctl LED_ON");
			break;

		case 't': /* toggle-led */
			if ((x = parse_led(optarg)) != -1)
				if (ioctl(ppldd_dev, PPLDD_IOC_LED_TOG, x) == -1)
					die("ioctl LED_TOG");
			break;

		case 'g': /* get-leds */ {
			int i;
			x = 0;
			if (ioctl(ppldd_dev, PPLDD_IOC_LED_GET, (ppldd_led_stat_t*)&x) == -1)
				die("ioctl LED_GET");
			if (optarg && tolower(*optarg) == 'h')
			{
				unsigned char w;
				for (i = ((PPLDD_LED_COUNT-1)&~3); i >= 0; i-=4)
				{
					w = x >> i & 15;
					putchar(w < 10 ? '0' + w : 'A' - 10 + w);
				}
			}
			else
			{
				for (i = PPLDD_LED_COUNT-1; i >= 0; i--)
					putchar(x >> i & 1 ? '1' : '0');
			}
			putchar('\n');
			break; }

		case 's': /* set-leds */
			if ((x = parse_leds(optarg)) != -1)
				if (ioctl(ppldd_dev, PPLDD_IOC_LED_SET, (ppldd_led_stat_t)x) == -1)
					die("ioctl LED_SET");
			break;

#ifdef PPLDD_ENABLE_LCD
		/********************** LCD */
		case 'w':
			opt_wrap = 1;
			break;

		case 'z': /* clear-lcd */
			if (ioctl(ppldd_dev, PPLDD_IOC_LCD_CLEAR) == -1)
				die("ioctl LCD_CLEAR");
			break;
			
		case 'y': /* row */
			x = atoi(optarg);
			if (x < 0 || x >= PPLDD_LCD_ROWS)
			{
				fprintf(stderr, "%s: row %d out of range (0..%d)\n", progname, x, PPLDD_LCD_ROWS-1);
				return EDOM;
			}
			lcd_row = x;
			break;

		case 'x': /* column */
			x = atoi(optarg);
			if (x < 0 || x >= PPLDD_LCD_COLS)
			{
				fprintf(stderr, "%s: col %d out of range (0..%d)\n", progname, x, PPLDD_LCD_COLS-1);
				return EDOM;
			}
			lcd_col = x;
			break;

		case 'c': /* character */
			x = atoi(optarg);
			if (x < 0 || x >= 256)
			{
				fprintf(stderr, "%s: char %d out of range (0..255)\n", progname, x);
				return EDOM;
			}
			else
			{
				struct ppldd_lcd_char req = { lcd_row, lcd_col, x };
				if (ioctl(ppldd_dev, PPLDD_IOC_LCD_SETCHAR, req) == -1)
					die("ioctl LCD_SETCHAR");
			}
			break;

		case 'm': /* string */
			write_lcd(optarg, strlen(optarg));
			break;

		case 'f': /* file */ {
			FILE *in;
			char data[MAX_DATALEN];
			int datalen;

			if (!optarg)
				in = stdin;
			else if (!(in = fopen(optarg, "r")))
			{
				fprintf(stderr, "%s: %s: %m\n", progname, optarg);
				return errno;
			}

			datalen = fread(data, 1, MAX_DATALEN, in);
			if (in != stdin)
				fclose(in);
			write_lcd(data, datalen);
		        break; }
#endif

		case ARGP_KEY_ARG: return ARGP_ERR_UNKNOWN; /* "fall-through" */
		case ARGP_KEY_ARGS: 
			break;

		default:
			return ARGP_ERR_UNKNOWN;
	}
	return 0;
}


int main(int argc, char **argv)
{
	progname = argv[0];
	if ((errno = argp_parse(&argp_parser, argc, argv, ARGP_IN_ORDER, 0, 0)))
		die("argp");
	exit(0);
}
