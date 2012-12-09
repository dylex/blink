#ifndef LIST_H
#define LIST_H

#include <stddef.h>
#include <assert.h>

#define HLIST_NEXT(TYPE) TYPE *next, **prev
#define HLIST_HEAD(TYPE) TYPE *

#define for_hlist(v, HEAD) for (v = (HEAD); v; v = v->next)
#define for_hlist_p(p, HEAD) for (p = &(HEAD); *p; p = &(*p)->next)

#define hlist_ins(X, NEXT) ({ \
		assert(!(X)->next && !(X)->prev); \
		if (((X)->next = (NEXT))) \
			(X)->next->prev = &(X)->next; \
		(X)->prev = &(NEXT); \
		NEXT = X; \
	})

#define hlist_del(X) ({ \
		if ((*(X)->prev = (X)->next)) { \
			(X)->next->prev = (X)->prev; \
			(X)->next = NULL; \
		} \
		(X)->prev = NULL; \
	})

#endif
