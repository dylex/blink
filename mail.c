#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <dirent.h>
#include "activity.h"
#include "notify.h"

#define MAIL_DIR	"mail/spool/new"
#define MAIL_COLOR	{ 0, COLOR_MAX/2, 0 }

static color_t Mail_color = MAIL_COLOR;
static unsigned Mail_count;

static void mail_update()
{
	static bool cur;
	if (Mail_count && !cur)
		base_add(Mail_color);
	else if (!Mail_count && cur)
		base_rm(Mail_color);
	cur = Mail_count;
}

static inline bool mail_inc(const char *name)
{
	return name && *name && name[0] != '.';
}

static int mail_refresh()
{
	Mail_count = 0;
	DIR *d = opendir(MAIL_DIR);
	if (!d)
		return -1;
	struct dirent *f;
	while ((f = readdir(d)))
		if (mail_inc(f->d_name))
			Mail_count ++;
	closedir(d);
	mail_update();
	return 0;
}

static const uint32_t Mask_new = IN_CREATE | IN_MOVED_TO;
static const uint32_t Mask_old = IN_DELETE | IN_MOVED_FROM;

static void mail_notify(struct notify *n, struct inotify_event *in)
{
	if (mail_inc(in->name))
	{
		if (in->mask & Mask_new)
			Mail_count ++;
		if (in->mask & Mask_old && Mail_count)
			Mail_count --;
	}
	mail_update();
}

static struct notify Mail_notify = { .fun = mail_notify };

int mail_init()
{
	if (notify_add(&Mail_notify, MAIL_DIR, Mask_new | Mask_old) < 0)
		return -1;
	return mail_refresh();
}
