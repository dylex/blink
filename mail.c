#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <dirent.h>
#include "activity.h"
#include "notify.h"
#include "remote.h"
#include "mail.h"

#define MAIL_DIR	"mail/spool/new"
#define MAIL_LED	LED_LOAD_1

static const color_t Mail_color[2] = { { 0, 0, COLOR_MAX/2 } , { 0, COLOR_MAX/2, 0 } };
signed Mail_count = 0;

void mail_update()
{
	base_set(Mail_color[!!Mail_count], MAIL_LED);
	remote_update();
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
