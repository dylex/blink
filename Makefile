CC=gcc
CFLAGS=-O -Wall -g -D_GNU_SOURCE=1

FILES=blink1d mail loadavg control activity notify watch blink1

default: blink1d blink1c

blink1d: $(addsuffix .o,$(FILES))
blink1c: blink1c.o

clean:
	rm -f *.o blink1c blink1d

.depend:
	gcc -MM $(addsuffix .c,$(FILES)) blink1c.c > $@

include .depend
