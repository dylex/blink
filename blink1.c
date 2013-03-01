#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <sys/ioctl.h>
#include <linux/hidraw.h>

#define HIDDEV_DIR	"/sys/bus/hid/devices"
#define BLINK1_VENDOR	0x27B8
#define BLINK1_PRODUCT	0x01ED

static int blink1_open_try(const char *path)
{
	int d = open(path, O_RDWR|O_NONBLOCK);
	if (d < 0)
		return d;

	struct hidraw_devinfo info = {};
	if (ioctl(d, HIDIOCGRAWINFO, &info) < 0)
	{
		close(d);
		return -1;
	}

	if (info.vendor != BLINK1_VENDOR || info.product != BLINK1_PRODUCT)
	{
		close(d);
		errno = ENXIO;
		return -1;
	}

	return d;
}

int blink1_open(const char *path)
{
	if (path)
		return blink1_open_try(path);

	static char hidpath[512];
	int d = -1;
	DIR *hiddir = opendir(HIDDEV_DIR);
	if (!hiddir)
		return -1;
	struct dirent *ent;
	while (d < 0 && (ent = readdir(hiddir)))
	{
		unsigned short int vend, prod;
		if (sscanf(ent->d_name, "%*4x:%4hx:%4hx.", &vend, &prod) != 2
				|| vend != BLINK1_VENDOR || prod != BLINK1_PRODUCT)
			continue;
		snprintf(hidpath, sizeof(hidpath), "%s/%s/hidraw", HIDDEV_DIR, ent->d_name);
		DIR *rawdir = opendir(hidpath);
		if (!rawdir)
			continue;
		while (d < 0 && (ent = readdir(rawdir)))
		{
			if (strncmp(ent->d_name, "hidraw", 6))
				continue;
			snprintf(hidpath, sizeof(hidpath), "/dev/%s", ent->d_name);
			d = blink1_open_try(hidpath);
		}
		closedir(rawdir);
	}
	closedir(hiddir);
	if (d < 0)
		errno = ENODEV;
	return d;
}

void blink1_close(int d)
{
	close(d);
}

static int blink1_write(int d, const void *buf, size_t len)
{
	return write(d, buf, len);
	return ioctl(d, HIDIOCSFEATURE(len), buf);
}

#define blink1_cmd(D, CMD, ARGS...) ({ \
		uint8_t _cmd[8] = { 1, CMD, ##ARGS }; \
		blink1_write(D, _cmd, sizeof(_cmd)); \
	})

int blink1_set(int d, uint8_t r, uint8_t g, uint8_t b)
{
	if (d < 0)
	{
		printf("blink1_set(#%02x%02x%02x)\n", r, g, b);
		return d;
	}
	return blink1_cmd(d, 'n', r, g, b);
}

int blink1_fade(int d, uint16_t t, uint8_t r, uint8_t g, uint8_t b)
{
	if (d < 0)
	{
		printf("blink1_fade(%hu, #%02x%02x%02x)\n", t, r, g, b);
		return d;
	}
	return blink1_cmd(d, 'c', r, g, b, t >> 8, t & 0xFF);
}
