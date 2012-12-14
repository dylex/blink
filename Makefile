CC=gcc
CFLAGS=-O -Wall -g -D_GNU_SOURCE=1
LDFLAGS=-lm
BINDIR=/usr/bin

FILES=blinkd mail loadavg command activity notify watch blink1
PROGS=blinkd blink

default: $(PROGS)

blinkd: $(addsuffix .o,$(FILES))
blink: blink.o

clean:
	rm -f *.o blink blink1d

.depend: $(addsuffix .c,$(FILES)) blink.c
	gcc -MM $^ > $@

install: $(PROGS)
	install -t $(BINDIR) $^

include .depend
