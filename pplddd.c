#include <sys/io.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <utmp.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "ppldd.h"

#define MAILFILE	"/home/dylan/mail/spool"
#define LOADAVGFILE	"/proc/loadavg"
#define TEMPFILE	"/sys/bus/i2c/devices/0-0290/temp2_input"
#define TEMPWARN	55000
#define PINGPROG	"/usr/sbin/fping -eA -r 1 64.81.235.104 128.242.125.65"

#define LOADLEDNUM	1
#define USERSLEDNUM	3
#define TEMPLEDNUM	4
#define MAILLEDNUM	5
#define PINGLEDNUM	8

int ppldd_dev;
char *progname;

void die(const char *) __attribute__((noreturn));
inline void led_on(int);
inline void led_off(int);
inline void led_tog(int);

void init_leds();
int checkfile(char *);
int loadavg();
inline long load_delay();
int temp();
void users();
int pingstat();

int main(int argc, char **argv)
{
	struct timeval tv;
	long ld;
	int i;

	progname=argv[0];

	if ((ppldd_dev = open("/dev/" PPLDD_DEVICE, O_RDONLY)) == -1)
		die("open /dev/" PPLDD_DEVICE);

	init_leds();
	led_on(LOADLEDNUM);

	while (1) {
		gettimeofday(&tv, NULL);
		/* load avg */
		while ((tv.tv_sec % 60)*1000000L + tv.tv_usec + (ld = load_delay()) < 59000000L)
		{
			usleep(ld);
			led_tog(LOADLEDNUM);
			/* outb(ledtime(), PPPORT); */
			usleep((ld < 1000000L) ? ld : 1000000L);
			led_tog(LOADLEDNUM);
			/* n*100 => 1/n; 100 => 1; 50 => 2; 20 => 5; 10 => 10; 5 => 20; ~0 => 59 (+1 = 60) */
			gettimeofday(&tv, NULL);
		}
		usleep(60000000L - (tv.tv_sec % 60)*1000000L - tv.tv_usec);
		/* users */
		users();
		/* ping time */
		led_on(PINGLEDNUM);
		i = pingstat();
		if (i >= 0)
		{
			if (i > 5000)
				usleep(5000000L);
			else
				usleep(i * 1000L);
			led_off(PINGLEDNUM);
		}
		/* temp limit */
#ifdef TEMPWARN
		if (temp() > TEMPWARN)
		{
			led_on(TEMPLEDNUM);
		}
		else
		{
			led_off(TEMPLEDNUM);
		}
#endif
		/* mail */
		if (checkfile(MAILFILE))
		{
			led_on(MAILLEDNUM);
		}
		else
		{
			led_off(MAILLEDNUM);
		}
	}
	/* hmm, what're we doing here? */
	return(-1);
}

void die(const char *err)
{
	fprintf(stderr, "%s: %s: %m\n", progname, err);
	close(ppldd_dev);
	exit(1);
}

inline void led_on(int led)
{
	if (ioctl(ppldd_dev, PPLDD_IOC_LED_ON, led) == -1)
		die("ioctl LED_ON");
}
inline void led_off(int led)
{
	if (ioctl(ppldd_dev, PPLDD_IOC_LED_OFF, led) == -1)
		die("ioctl LED_OFF");
}
inline void led_tog(int led)
{
	if (ioctl(ppldd_dev, PPLDD_IOC_LED_TOG, led) == -1)
		die("ioctl LED_TOG");
}

#define INIT_LED_DELAY 125000L
void init_leds()
{
	int led = 1;
	led_tog(led);
	usleep(INIT_LED_DELAY);
	for (led = 2; led <= PPLDD_LED_COUNT; led++)
	{
		led_tog(led);
		usleep(INIT_LED_DELAY);
		led_tog(led-1);
	}
	usleep(INIT_LED_DELAY);
	led_tog(led-1);
}

int checkfile(char *filename)
{
	struct stat s;

	if (stat(filename, &s))
	{
		perror(filename);
		return 0;
	}
	return (s.st_mtime >= s.st_atime);
}

int loadavg()
{
	FILE *f;
	int la = 0, c;

	if (!(f = fopen(LOADAVGFILE, "r")))
	{
		perror(LOADAVGFILE);
		return 0;
	}
	while (((c = fgetc(f)) != ' ') && (c != EOF));
	while (((c = fgetc(f)) != ' ') && (c != EOF))
	{
		if (c != '.')
		{
			la *= 10;
			la += c - '0';
		}
	}
	fclose(f);
	return la;
}

inline long load_delay()
{
	int la = loadavg();
	return la ? 100000000L / la : 100000000L;
}

int temp()
{
	FILE *f;
	int t = 0, c;

	if (!(f = fopen(TEMPFILE, "r")))
	{
		perror(TEMPFILE);
		return 0;
	}
	/*
	do {
		c = fgetc(f);
		if (c == EOF)
		{
			fprintf(stderr, "Error parsing %s\n", TEMPFILE);
			return 0;
		}
		if (c == ' ')
			i++;
	} while (i < 2);
	*/

	while (('0' <= (c = fgetc(f))) && (c <= '9') && (c != EOF))
	{
		t *= 10;
		t += c - '0';
	}
	fclose(f);
	return t;
}

void users()
{
	struct utmp *u;
	struct stat ttystat;
	time_t t = time(NULL);

	setutent();
	while ((u = getutent())) {
		if ((u->ut_type == USER_PROCESS) && (u->ut_user[0]) && strcmp(u->ut_user, "dylan"))
		{
			char tty[strlen("/dev/") + strlen(u->ut_line) + 1];
			int idle = 0;
			strcpy(tty, "/dev/");
			strcpy(tty+strlen("/dev/"), u->ut_line);
			if (stat(tty, &ttystat) == -1)
				fprintf(stderr, "stat(%s): %m\n", tty);
			else
				idle = t - ttystat.st_atime;

			led_on(USERSLEDNUM);
			usleep(idle <= (5*60) ? 50000L : ((idle >= 100*60) ? 1000000L : (500L*idle)/3));
			led_off(USERSLEDNUM);
			usleep(250000L);
		}
	}
	endutent();
	led_off(USERSLEDNUM);
}

int pingstat()
{
	FILE *f;
	int tt=0;
	char s[256], *p;

	if (!(f = popen(PINGPROG, "r")))
	{
		perror(PINGPROG);
		return 0;
	}
	while (fgets(s, 255, f))
	{
		if ((p = strstr(s, "is alive (")))
		{
			int t = 0;
			for (p += 10; (p < s + 255) && (((*p <= '9') && (*p >= '0')) || (*p == '.')); p++)
			{
				if (*p != '.')
				{
					t *= 10;
					t += *p - '0';
				}
			}
			tt += t;
		}
	}
	return pclose(f) ? -1 : tt;
}
