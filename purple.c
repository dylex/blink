#include "activity.h"
#include "remote.h"
#include "purple.h"

#define PURPLE_COLOR	{ COLOR_MAX/2, COLOR_MAX/2, 0 }
#define PURPLE_TIME	(INTERVAL_SECOND/2)

signed Purple_count = -1;

static activity_fn purple_next;

static struct activity Purple_act = { .fun = &purple_next };
static struct activity_then Purple_blink = { { { .start = PURPLE_COLOR, .end = PURPLE_COLOR, .len = PURPLE_TIME }, .fun = activity_then, .led = LED_1 }, &Purple_act };

static void purple_next(struct activity *act)
{
	static unsigned count = 0;

	if (Purple_count > 0) {
		Purple_act.seg.len = PURPLE_TIME;
		if (++count >= Purple_count) {
			count = 0;
			Purple_act.seg.len *= 3;
		}
		activity_add(&Purple_blink.act);
	}
	else
		count = 0;
}

void purple_update(signed count)
{
	if (Purple_count == count)
		return;
	Purple_count = count;
	if (count == -1) {
		if (activity_active(&Purple_act))
			activity_rm(&Purple_act, NULL);
	}
	else if (count > 0 && !(activity_active(&Purple_blink.act) || activity_active(&Purple_act)))
		purple_next(NULL);
	remote_update();
}
