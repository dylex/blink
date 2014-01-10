#ifndef WATCH_H
#define WATCH_H

#include <stdbool.h>
#include "types.h"
#include "list.h"

enum watch_event {
	WATCH_IN  = 0,
	WATCH_OUT,
	WATCH_EXC,
	WATCH_EVENTS
};
#define WATCH_EVENT(W) (1U << (W))
#define WATCH(T) WATCH_EVENT(WATCH_##T)

struct watch;
typedef void watch_fn(struct watch *, uint8_t events);
struct watch {
	int fd;
	uint8_t events;
	watch_fn *fun;

	HLIST_NEXT(struct watch);
};

void watch_add(struct watch *);
void watch_rm(struct watch *);
bool watch_active(const struct watch *);
int watch_run(interval_t d);

#endif
