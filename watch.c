#include <sys/select.h>
#include <assert.h>

#include "activity.h"
#include "watch.h"

static HLIST_HEAD(struct watch) Watch_list;
static fd_set Watch_sets[WATCH_EVENTS];
static int Watch_max = 0;

#define for_event(e) for (e = 0; e < WATCH_EVENTS; e++)

static void watch_set(struct watch *w)
{
	assert(w->fd >= 0);
	if (w->fd >= Watch_max)
		Watch_max = w->fd + 1;
	enum watch_event e;
	for_event (e)
		if (w->events & WATCH_EVENT(e))
			FD_SET(w->fd, &Watch_sets[e]);
}

static void watch_refresh()
{
	enum watch_event e;
	Watch_max = 0;
	for_event (e)
		FD_ZERO(&Watch_sets[e]);
	struct watch *w;
	for_hlist (w, Watch_list)
		watch_set(w);
}

void watch_add(struct watch *w)
{
	hlist_ins(w, Watch_list);
	watch_set(w);
}

void watch_rm(struct watch *w)
{
	hlist_del(w);
	watch_refresh();
}

bool watch_active(const struct watch *w)
{
	return hlist_on_list(w);
}

int watch_run(interval_t t)
{
	enum watch_event e;
	fd_set set[WATCH_EVENTS];
	for_event (e)
		set[e] = Watch_sets[e];
	struct timeval tv = { t / INTERVAL_SECOND, t % INTERVAL_SECOND * (1000000L / INTERVAL_SECOND) };
	int r = select(Watch_max, &set[WATCH_IN], &set[WATCH_OUT], &set[WATCH_EXC], t == INTERVAL_INF ? NULL : &tv);
	if (r < 0)
		return r;
	if (t != INTERVAL_INF)
		t -= tv.tv_sec * INTERVAL_SECOND + tv.tv_usec / (1000000L / INTERVAL_SECOND);
	active_pop(t);

	if (!r)
		return 0;
	struct watch *w;
	for_hlist (w, Watch_list)
	{
		uint8_t r = 0;
		for_event (e)
			if (w->events & WATCH_EVENT(e) && FD_ISSET(w->fd, &set[e]))
				r |= WATCH_EVENT(e);
		if (r)
			w->fun(w, r);
	}
	return 0;
}
