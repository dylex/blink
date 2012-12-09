#include <sys/inotify.h>
#include <unistd.h>
#include <stdio.h>
#include <limits.h>

#include "watch.h"
#include "notify.h"

static int INotify_fd = -1;
static HLIST_HEAD(struct notify) Notify_list;

int notify_add(struct notify *n, const char *path, uint32_t mask)
{
	n->wd = inotify_add_watch(INotify_fd, path, mask);
	if (n->wd < 0)
		return n->wd;
	hlist_ins(n, Notify_list);
	return 0;
}

int notify_rm(struct notify *n)
{
	assert(n->wd >= 0);
	int r = inotify_rm_watch(INotify_fd, n->wd);
	if (r < 0)
		return r;
	hlist_del(n);
	return 0;
}

static int inotify_read(void *buf, size_t len)
{
	ssize_t r = read(INotify_fd, buf, len);
	if (r == 0)
		return 0;
	if (r < 0)
	{
		fprintf(stderr, "inotify read: %m\n");
		return -1;
	}
	if (r != len)
	{
		fprintf(stderr, "inotify read: %zd\n", r);
		return -2;
	}
	return 1;
}

static void notify_run(struct watch *w, uint8_t events)
{
	union {
		struct inotify_event ev;
		char buf[sizeof(struct inotify_event) + NAME_MAX + 1];
	} evbuf;
	if (inotify_read(&evbuf.ev, sizeof evbuf.ev) <= 0)
		return;
	if (evbuf.ev.len > sizeof(evbuf) - sizeof(evbuf.ev))
	{
		fprintf(stderr, "inotify len: %u\n", evbuf.ev.len);
		return;
	}
	if (evbuf.ev.len && inotify_read(evbuf.ev.name, evbuf.ev.len) <= 0)
		return;

	struct notify *n;
	for_hlist (n, Notify_list)
		if (n->wd == evbuf.ev.wd)
			n->fun(n, &evbuf.ev);
}

static struct watch Notify_watch = { .events = WATCH(IN), .fun = &notify_run };

int notify_init()
{
	INotify_fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
	if (INotify_fd < 0)
		return INotify_fd;
	Notify_watch.fd = INotify_fd;
	watch_add(&Notify_watch);
	return 0;
}
