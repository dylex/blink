#ifndef NOTIFY_H
#define NOTIFY_H

#include <sys/inotify.h>
#include "list.h"

struct notify;
typedef void notify_fn(struct notify *, struct inotify_event *);
struct notify {
	notify_fn *fun;
	int wd;

	HLIST_NEXT(struct notify);
};

int notify_add(struct notify *, const char *, uint32_t);
int notify_rm(struct notify *);
int notify_init();

#endif
