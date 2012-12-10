CC=gcc
CFLAGS=-O -Wall -g -D_GNU_SOURCE=1

FILES=blink1d mail loadavg activity notify watch blink1

blink1d: $(addsuffix .o,$(FILES))

clean:
	rm -f *.o blink1c blink1d

.depend:
	gcc -MM $(addsuffix .c,$(FILES)) > $@

include .depend
