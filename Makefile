CC=gcc
CFLAGS=-O -Wall -g -D_GNU_SOURCE=1
LDFLAGS=-lm

FILES=blink1d mail loadavg command activity notify watch blink1

default: blink1d blink

blink1d: $(addsuffix .o,$(FILES))
blink: blink.o

clean:
	rm -f *.o blink blink1d

.depend: $(addsuffix .c,$(FILES)) blink.c
	gcc -MM $^ > $@

include .depend
